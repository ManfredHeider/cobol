      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   WUKI0301.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/WUKI0301.CPv  $
      *
      *   Rev 2.1   Aug 20 2018 08:06:18   t13ae31
      *AKI01619: SUH KSDS VSAM in DB2 BMG OZP
      *
      *   Rev 1.1   09 May 1998 14:13:00   P13SP99
      *Neuerstellung
      *
      *   Rev 1.0   Apr 18 1997 10:06:10   P13PVCS
      *Initial revision.
      *
      *---------------------------------------------------------------*
      ******************************************************************
      * COBOL DECLARATION FOR TABLE                                    *
      * 'AKI00001 V006.000 19980325'
      ******************************************************************
        05  WU-WUKI0301.
           10 G10SUHBMGVNR                  PIC  X(0015).
           10 G10SUHBMGKZ                   PIC  X(0001).
           10 G10SUHBMGTEXT                 PIC  X(0030).
           10 ZUGANG                        PIC  X(0032).
           10 VON                           PIC  X(0032).
           10 BIS                           PIC  X(0032).
           10 DCOP                          PIC  X(0001).
           10 NIL-DCOP                      PIC  X(01).

