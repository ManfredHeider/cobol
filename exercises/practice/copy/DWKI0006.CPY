      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DWKI0006.CPY  $
      *    $Revision:   2.0  $
      *
      *    $Log:   L:\MF\CPY\DWKI0006.CPv  $
      *
      *   Rev 2.0   Feb 27 1999 12:59:18   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.1   26 Mar 1998 10:20:04   P13SP13
      *Neuerstellung
      *
      *   Rev 1.0   24 Mar 1998 10:53:50   P13SP99
      *Initial revision.
      *
      *---------------------------------------------------------------*
      * --------------------------------------------------------------
      * 13-byte id generation
      * parameters for upki0015
      * --------------------------------------------------------------
       01  dwki0006-param.
           03  dwki0006-id.
               10  dwki0006-identifier     pic 9(08).
               10  dwki0006-identifier-red redefines
                                           dwki0006-identifier.
                   15  dwki0006-ges        pic 9(4).
                   15  dwki0006-fill       pic 9(4).
           03  dwki0006-outid              pic x(13).
           03  dwki0006-timestamp          pic x(26).
           03  dwki0006-return-code        pic s9.
