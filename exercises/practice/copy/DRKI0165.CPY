      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   drki0165.cpy  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:\MF\CPY\drki0165.cpv  $
      *
      *   Rev 2.1   Apr 16 1999 09:52:48   I13PC19
      *AKI00027
      *Produktionsübergabe
      *
      *   Rev 2.0   Feb 27 1999 12:53:24   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.0   29 Sep 1997 11:53:42   P13SP99
      *Initial revision.
      *
      *---------------------------------------------------------------*


      *
      * THESE ARE PARAMETERS PASSED FOR CREATION OF ACTION RECORDS
      * FOR PROGRAM UPKI0034 PC MIGRATION
      *

       01  DRKI0165-PARAM.
           05  DRKI0165-FUNCTION-CODE    PIC X(1).
           05  DRKI0165-PROGRAM-FROM     PIC X(8).
           05  DRKI0165-FILE-NAME        PIC X(8).
           05  DRKI0165-FILE-ACTION-CODE PIC X(1).
           05  DRKI0165-FUNCTION-2       PIC X(9).
           05  DRKI0165-FILE-REST.
               10  DRKI0165-REST-2       PIC X(1100).
           05  DRKI0165-RETURN-CODE      PIC S99.
