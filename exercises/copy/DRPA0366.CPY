      *---------------------------------------------------------------*
      *    Standard f?r alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRPA0366.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:\MF\CPY\DRPA0366.CPv  $
      *
      *   Rev 2.1   May 28 1999 18:30:58   T13EX26
      *Prod-Übergabe 28.0598 hpa
      *
      *   Rev 2.0   May 26 1999 16:57:20   T13EX26
      *MFE-Umstellung
      *
      *   Rev 1.0   09 Jul 1997 16:04:14   P13PVCS
      *Initial revision.
      *
      *---------------------------------------------------------------*
      *---------------------------------------------------------------*
      ***************************************************************
      *                 PARAMETER-INPUT                             *
      ***************************************************************
       01  PI-DRPA0366.
           05  PI-DRPA0366-VBNR               PIC 9(9)  BINARY.
           05  PI-DRPA0366-VNR                PIC X(15).

      ***************************************************************
      *                 PARAMETER-OUTPUT                            *
      ***************************************************************
       01  PO-DRPA0366.
           05  PO-DRPA0366-RC                    PIC  X(03).
             88  PO-DRPA0366-RC-NICHT-BERECHT    VALUE 'NBR'.
             88  PO-DRPA0366-RC-BERECHTIGT       VALUE 'KAN','OK '.
             88  PO-DRPA0366-RC-KEIN-ANTRAG      VALUE 'KAN'.
             88  PO-DRPA0366-RC-BEANTRAGT        VALUE 'ANT'.
      *
      ******** ENDE COPY-MEMBER (DRPA0366) *******************
