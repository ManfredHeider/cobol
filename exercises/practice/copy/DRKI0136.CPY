      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI0136.CPY  $
      *    $Revision:   2.0  $
      *
      *    $Log:   L:\MF\CPY\DRKI0136.CPv  $
      *
      *   Rev 2.0   Feb 27 1999 12:53:08   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.1   12 May 1998 11:59:48   P13SP99
      *aki00001 - Neuerstellung
      *
      *   Rev 1.0   29 Sep 1997 11:50:44   P13SP99
      *Initial revision.
      *
      *---------------------------------------------------------------*
      *
      * suchname fields
      *
      *
      *
      *
       01  DRKI0136-gesamt.
           03 z-suchnamen.
               05 z-suchname-mit-pkz       pic x(11).
               05 z-suchname-ohne-pkz      pic x(10).
               05 z-suchvorname-mit-pkz    pic x(11).
               05 z-suchvorname-ohne-pkz pic x(10).
               05 z-suchname-voll          pic x(10).
               05 z-suchvorname-voll       pic x(10).
               05 z-suchplz                pic x(15).
               05 z-suchgeburtstag         pic x(10).
      *
