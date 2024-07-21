      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRAT0315.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:\MF\CPY\DRAT0315.CPv  $
      *
      *   Rev 2.1   Jan 21 2000 13:52:58   T13EX44
      * 
      *
      *   Rev 2.0   Mar 16 1999 09:57:24   P13SP20
      *Archiverstellung
      *
      *---------------------------------------------------------------*

          01 PI-DRAT0315.
           05 PI-DRAT0315-DATEN.
            10 PI-DRAT0315-ZAHL        PIC  9(015).
      *
          01 PO-DRAT0315.
           05 PO-DRAT0315-RC           PIC  X(003).
              88 PO-DRAT0315-RC-OK     VALUE 'OK '.
              88 PO-DRAT0315-RC-NOK    VALUE 'NOK'.
           05 PO-DRAT0315-DATEN.
            10 PO-DRAT0315-RC-MOD-11   PIC  X(003).
            10 PO-DRAT0315-PZ-MOD-11   PIC  9(001).

