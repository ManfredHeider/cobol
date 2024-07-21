      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI0110.CPY  $
      *    $Revision:   2.0  $
      *
      *    $Log:   L:\MF\CPY\DRKI0110.CPv  $
      *
      *   Rev 2.0   Feb 27 1999 12:52:54   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.1   26 Mar 1998 10:19:50   P13SP13
      *Neuerstellung
      *
      *   Rev 1.0   29 Sep 1997 11:48:00   P13SP99
      *Initial revision.
      *
      *---------------------------------------------------------------*
      * --------------------------------------------------------------
      * 13-byte id generation
      * parameters for upki0015
      * --------------------------------------------------------------
       01  drki0110-param.
           03  drki0110-id.
               10  drki0110-identifier     pic 9(08).
               10  drki0110-identifier-red redefines
                                           drki0110-identifier.
                   15  drki0110-ges        pic 9(4).
                   15  drki0110-fill       pic 9(4).
           03  drki0110-outid              pic x(13).
           03  drki0110-timestamp          pic x(26).
           03  drki0110-return-code        pic s9.
