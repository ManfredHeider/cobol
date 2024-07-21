      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI0805.CPY  $
      *    $Revision:   2.0  $
      *
      *    $Log:   L:\MF\CPY\DRKI0805.CPv  $
      *
      *   Rev 2.0   Feb 27 1999 12:58:42   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.1   09 Feb 1999 12:10:40   T13EX40
      *AKI00016 - Neuerstellung
      *
      *   Rev 1.0   12 Jan 1999 15:11:24   P13SP19
      *Initial revision.
      *
      *---------------------------------------------------------------*
       01  PI-DRKI0805.
           05  PI-DRKI0805-UFKT               PIC X.
               88 PI-DRKI0805-PERSONID          VALUE 'P'.
               88 PI-DRKI0805-VERTRAGID         VALUE 'V'.
               88 PI-DRKI0805-OBJEKTID          VALUE 'O'.
               88 PI-DRKI0805-ADRESSEID          VALUE 'A'.
      *         88 PI-DRKI0805-PCNUMMER          VALUE 'N'.
           05  PI-DRKI0805-SUCHID             PIC X(13).

      *
       01  PO-DRKI0805.
           05  PO-DRKI0805-VBID               PIC X(13).
           05  PO-DRKI0805-RC                 PIC  X(003).
               88 PO-DRKI0805-RC-OK                        VALUE 'OK '.
               88 PO-DRKI0805-RC-NOT-FOUND                 VALUE 'NFD'.
      *
      ******* ENDE COPY-MEMBER **************************************
