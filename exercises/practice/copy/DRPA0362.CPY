      *---------------------------------------------------------------*
      *    Standard f?r alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRPA0362.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:\MF\CPY\DRPA0362.CPv  $
      *
      *   Rev 2.1   May 28 1999 18:30:52   T13EX26
      *Prod-Übergabe 28.0598 hpa
      *
      *   Rev 2.0   May 26 1999 16:57:08   T13EX26
      *MFE-Umstellung
      *
      *   Rev 1.0   09 Jul 1997 16:03:34   P13PVCS
      *Initial revision.
      *
      *
      *---------------------------------------------------------------*
      ***************************************************************
      *                 PARAMETER-INPUT                             *
      ***************************************************************
       01  PI-DRPA0362.
           05  PI-DRPA0362-VERTRAGS-ID        PIC X(13).
           05  PI-DRPA0362-ROLLE-KZ           PIC X(01).
           05  PI-DRPA0362-AV-KZ              PIC X(01).

      ***************************************************************
      *                 PARAMETER-OUTPUT                            *
      ***************************************************************
       01  PO-DRPA0362.
           05  PO-DRPA0362-RC                  PIC  X(03).
               88  PO-DRPA0362-RC-OK           VALUE 'OK '.
               88  PO-DRPA0362-RC-NOK          VALUE 'NOK'.
               88  PO-DRPA0362-RC-NOT-FOUND    VALUE 'NFD'.
           05 PO-DRPA0362-MITARBEITER-NR       PIC S9(09) BINARY.
           05 PO-DRPA0362-GUELTIGKEITSDAT      PIC  X(10).
      *
      ******** ENDE COPY-MEMBER (DRPA0362) *******************

