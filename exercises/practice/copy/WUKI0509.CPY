      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   WUKI0509.CPY  $
      *    $Revision:   2.3  $
      *
      *    $Log:   L:/mf/CPY/WUKI0509.CPv  $
      *
      *   Rev 2.3   Nov 21 2019 08:34:16   t13ex27
      *AKI01674: DSGVO: Mappen der DB2-Tabelle zu gesperrten/gelösc
      *hten Verträge in die OKI
      *
      *   Rev 2.2   Nov 28 2018 17:33:44   t13ac20
      *AKI01600: Sperren & Löschen von Verträgen - DB2 neue Tabellen
      *
      *   Rev 2.1   Sep 11 2018 12:17:42   t13ac20
      *AKI01600: Sperren & Löschen von Verträgen - DB2 neue Tabellen
      *
      *   Rev 1.1   09 May 1998 14:15:44   P13SP99
      *Neuerstellung
      *
      *   Rev 1.0   15 Jan 1998 18:07:10   P13SP13
      *Initial revision.
      *
      *---------------------------------------------------------------*
      ******************************************************************
      * COBOL DECLARATION FOR TABLE                                    *
      * 'AKI00001 V006.000 19980325'
      ******************************************************************
        05  WU-WUKI0509.
           10 SPAVERTRAGSID                 PIC  X(0013).
           10 SPAVERTRAGSNR                 PIC  X(0015).
           10 SPAPARTGESELLID               PIC S9(4) COMP.
           10 SPASTATUS-KTY                 PIC S9(4) COMP.
           10 SPASTATUS-KNR                 PIC S9(4) COMP.
           10 SPAGRUND                      PIC  X(0001).
           10 SPASPERRDAT                   PIC  X(010).
           10 SPAARCHIVTST                  PIC  X(0026).
           10 NIL-SPAARCHIVTST              PIC  X(01).
           10 SPALOESCHTST                  PIC  X(0026).
           10 NIL-SPALOESCHTST              PIC  X(01).
           10 SPAENTSPERRTST                PIC  X(0026).
           10 NIL-SPAENTSPERRTST            PIC  X(01).
           10 LETZTEAENDERUNG               PIC  X(0026).
           10 SPALFDNR                      PIC S9(4) COMP.

