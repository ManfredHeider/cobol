      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRRW0006.CPY  $
      *    $Revision:   2.0  $
      *
      *    $Log:   L:\MF\CPY\DRRW0006.CPv  $
      *
      *   Rev 2.0   Feb 27 1999 14:51:16   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.1   23 Sep 1998 12:29:24   T13AE35
      *AIS00009 - Neuerstellung, SST HV-Kontenstaende fuer FIBU
      *
      *   Rev 1.0   10 Mar 1998 17:18:54   P13SP99
      *Initial revision.
      *
      *---------------------------------------------------------------*
      *                                                               *
      *            COPY-MEMBER  Satzbschreibung HV-Kontenstände       *
      *                         (Importdatei ALLIS FIN/CO)            *
      *            DATEINUMMER:                                       *
      *            COPYMEMBER:  DRRW0006                              *
      *            SATZLAENGE:  176 Bytes                             *
      *                                                               *
      *                                                               *
      *---------------------------------------------------------------*
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * ARW00013!26.05.98!    ! NEU                       ! Moll     *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************

       01  AFC-HV-KTO-SATZ.
      *    * Feld 1: VB-Nummer (7st. + Prüfziffer)
           05  hvNummer                PIC  X(08).
           05  tz-01                   PIC  X(01).
           05  buchPeriode             PIC  X(06).
           05  tz-02                   PIC  X(01).
           05  direktion               PIC  X(08).
           05  tz-03                   PIC  X(01).
           05  waehrungsKz             PIC  X(03).
           05  tz-04                   PIC  X(01).
           05  betrDiskont             PIC  9(18).
           05  tz-05                   PIC  X(01).
           05  sollDiskont             PIC  9(01).
           05  tz-06                   PIC  X(01).
           05  betrRueckst             PIC  9(18).
           05  tz-07                   PIC  X(01).
           05  sollRueckst             PIC  9(01).
           05  tz-08                   PIC  X(01).
           05  betrVorsch              PIC  9(18).
           05  tz-09                   PIC  X(01).
           05  sollVorsch              PIC  9(01).
           05  tz-10                   PIC  X(01).
           05  betrAbschr              PIC  9(18).
           05  tz-11                   PIC  X(01).
           05  sollAbschr              PIC  9(01).
           05  tz-12                   PIC  X(01).
           05  betrAnwalt              PIC  9(18).
           05  tz-13                   PIC  X(01).
           05  sollAnwalt              PIC  9(01).
           05  tz-14                   PIC  X(01).
           05  betrGesamt              PIC  9(18).
           05  tz-15                   PIC  X(01).
           05  sollGesamt              PIC  9(01).
           05  tz-16                   PIC  X(01).
           05  jahrBasisSollSaldo      PIC  9(04).
           05  tz-17                   PIC  X(01).
           05  jahrErstAbschr          PIC  9(04).
           05  tz-18                   PIC  X(01).
           05  aenderungsKz            PIC  9(01).
           05  tz-19                   PIC  X(01).
           05  fehlerKz                PIC  9(01).
           05  tz-20                   PIC  X(01).
           05  stornoKz                PIC  9(01).
           05  tz-21                   PIC  X(01).
           05  funktionsKz             PIC  X(03).
           05  tz-22                   PIC  X(01).
           05  doppelKz                PIC  9(01).


