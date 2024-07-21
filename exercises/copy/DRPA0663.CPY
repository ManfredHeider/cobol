      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRPA0663.CPY  $
      *    $Revision:   2.0  $
      *
      *    $Log:   L:\MF\CPY\DRPA0663.CPv  $
      *
      *   Rev 2.0   Feb 27 1999 14:29:26   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.1   12 Nov 1998 10:34:24   T01JM2S
      *PEG-PRODUKTIONSüBERGABE
      *
      *   Rev 1.0   11 Nov 1997 16:58:34   P13SP99
      *Initial revision.
      *
      *---------------------------------------------------------------*
      *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER
      *    FÜR DIE FUNKTION DRPA0663: LESEN aktuellste Version zu Printe
      *    AUSSER: DEM FUNKTIONSNAMEN,
      *            DEM ERR-ERROR-BEREICH UND
      *            DEM STANDARD-STATUS-BEREICH
      *            (JEWEILS SEPARATE COPYS)
      *
      ****************************************************************
      *
       01  PI-DRPA0663.
           05  PI-DRPA0663-DATEN.
               10 PI-DRPA0663-PRINTE        PIC S9(015) PACKED-DECIMAL.
      *
       01  PO-DRPA0663.
           05  PO-DRPA0663-RC               PIC  X(003).
               88 PO-DRPA0663-RC-OK              VALUE 'OK '.
               88 PO-DRPA0663-RC-NOT-FOUND       VALUE 'NFD'.
           05  PO-DRPA0663-DATEN.
               10 PO-DRPA0663-VERSION       PIC S9(04) BINARY.
      *
      ******* ENDE COPY-MEMBER **************************************
