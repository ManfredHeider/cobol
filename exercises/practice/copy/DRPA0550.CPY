      *---------------------------------------------------------------*
      *    Standard f?r alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRPA0550.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:\MF\CPY\DRPA0550.CPv  $
      *
      *   Rev 2.1   Jun 02 1999 13:51:44   T13EX44
      * 
      *
      *   Rev 2.0   May 26 1999 16:57:44   T13EX26
      *MFE-Umstellung
      *
      *   Rev 1.0   01 Oct 1997 11:30:36   P13SP13
      *Initial revision.
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *
       01  PI-DRPA0550.
           05  PI-DRPA0550-PGEB-GES           PIC S9(4) BINARY.
           05  PI-DRPA0550-VNR                PIC X(15).
           05  PI-DRPA0550-ALT-VNR            PIC X(15).

      ***************************************************************
      *                 PARAMETER-OUTPUT                            *
      ***************************************************************
       01  PO-DRPA0550.
           05  PO-DRPA0550-RC                  PIC  X(03).
               88  PO-DRPA0550-RC-OK           VALUE 'OK '.
               88  PO-DRPA0550-RC-NOK          VALUE 'NOK'.
               88  PO-DRPA0550-RC-NOT-FOUND    VALUE 'NFD'.
               88  PO-DRPA0550-RC-KEIN-VERTRAG VALUE 'KVT'.
           05 PO-DRPA0550-AVERM                PIC S9(09) BINARY.
           05 PO-DRPA0550-KD-BETR              PIC S9(09) BINARY.
      *
      ******** ENDE COPY-MEMBER (DRPA0550) *******************

