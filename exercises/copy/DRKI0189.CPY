      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI0189.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:\MF\CPY\DRKI0189.CPv  $
      *
      *   Rev 2.1   Apr 16 1999 09:19:12   I13PC19
      *AKI00027
      *Produktionsübergabe
      *
      *   Rev 2.0   Feb 27 1999 12:53:34   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.0   29 Sep 1997 11:56:12   P13SP99
      *Initial revision.
      *
      *---------------------------------------------------------------*
      *
      * This file will be used to determine if the Client has been
      * already added to the data base.
      *

       01  DRKI0189-REC.
           05  DRKI0189-KEY.
               10 DRKI0189-CLIENT-ID            PIC X(13).
               10 DRKI0189-AGENT-ID             PIC X(13).
           05  DRKI0189-PC-NUMBER               PIC 9(05).
           05  DRKI0189-AGENT-NUMBER            PIC 9(07).
           05  DRKI0189-OLD-CLIENT-NUMBER       PIC 9(09).
