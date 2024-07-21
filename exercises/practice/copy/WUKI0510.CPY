      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   WUKI0510.CPY  $
      *    $Revision:   2.3  $
      *
      *    $Log:   L:/mf/CPY/WUKI0510.CPv  $
      *
      *   Rev 2.3   May 17 2019 11:51:36   t13ac20
      *AKI01600: Erweiterung Daisy
      *
      *   Rev 2.2   Nov 28 2018 17:34:16   t13ac20
      *AKI01600: Sperren & Löschen von Verträgen - DB2 neue Tabellen
      *
      *   Rev 2.1   Sep 11 2018 12:18:02   t13ac20
      *AKI01600: Sperren & Löschen von Verträgen - DB2 neue Tabellen
      *
      *   Rev 2.2   Dec 08 1999 13:06:50   T13EX41
      *FKI00288
      *- Tabellenänderungen STATISTIK, KISTATISTIK
      *
      *   Rev 2.1   Nov 15 1999 13:27:40   T13EX41
      *AKI00033
      *- DB2-Felder hinzugefügt
      *
      *   Rev 2.0   Feb 27 1999 13:01:10   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.1   09 May 1998 14:15:46   P13SP99
      *Neuerstellung
      *
      *   Rev 1.0   15 Jan 1998 18:07:18   P13SP13
      *Initial revision.
      *
      *---------------------------------------------------------------*
      ******************************************************************
      * COBOL DECLARATION FOR TABLE                                    *
      * 'AKI00001 V006.100 19991111'
      ******************************************************************
        05  WU-WUKI0510.
           10 SPSVERTRAGSNR                 PIC  X(0015).
           10 SPSPARTGESELLID               PIC S9(4) COMP.
           10 SPSLFDNR                      PIC S9(4) COMP.
           10 SPSVERTRAGSID                 PIC  X(0013).
           10 SPSERSTELLTTST                PIC  X(0026).
           10 SPSAKTION-KZN                 PIC  X(0001).
           10 LETZTEAENDERUNG               PIC  X(0026).
           10 SPSDAISY-KZN                  PIC  X(0001).

