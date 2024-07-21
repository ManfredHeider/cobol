      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI0180.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:\MF\CPY\DRKI0180.CPv  $
      *
      *   Rev 2.1   Sep 10 1999 12:47:32   T13EX12
      *FKI00140 DB-Korrektur IstVertragsAdresse
      *
      *   Rev 2.0   Feb 27 1999 12:53:30   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.0   29 Sep 1997 11:55:12   P13SP99
      *Initial revision.
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *    VERSION   01
      *    Copy Strecke für Restart des PMKI0332 Programmes
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * FKI00140!06.09.99! 01 ! NEUERSTELLUNG             !v.d.Goltz *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
       01  DRKI0180-RESTART.
           05 DRKI0180-COPY-NAME          PIC X(008)  VALUE 'DRKI0180'.
           05 DRKI0180-RESTART-ADRESSE-ID PIC X(013).
           05 DRKI0180-D-ANZ-VERROLLEB    PIC 9(009).
           05 DRKI0180-D-ANZ-ADRESSE      PIC 9(009).
           05 DRKI0180-D-ANZ-ADR-J        PIC 9(009).
           05 DRKI0180-D-ANZ-ADR-N        PIC 9(009).

