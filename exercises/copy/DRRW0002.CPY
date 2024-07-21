      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRRW0002.CPY  $
      *    $Revision:   2.0  $
      *
      *    $Log:   L:\MF\CPY\DRRW0002.CPv  $
      *
      *   Rev 2.0   Feb 27 1999 14:51:16   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.1   23 Sep 1998 12:25:20   T13AE35
      *AIS00009 - Neuerstellung, SST Stammdaten-Import fuer FIBU
      *
      *   Rev 1.0   10 Mar 1998 17:18:28   P13SP99
      *Initial revision.
      *
      *---------------------------------------------------------------*
      *                                                               *
      *            COPY-MEMBER  AllData StammdatenImport 'Partner'    *
      *            DATEINUMMER:                                       *
      *            COPYMEMBER:  DRRW0002                              *
      *            SATZLAENGE:  231 Bytes                             *
      *                                                               *
      *  Siehe:  ALLIS FINCO                                          *
      *          Verfahrensdokumentation Buchungen-Partnerimport      *
      *                                                               *
      *---------------------------------------------------------------*
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AAS00030!19.03.98!    ! NEU                       ! Moll     *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************

       01  AFC-PA-SATZ.
      *    * Feld 1: VB-Nummer (7st. + Prüfziffer) in Feld partner
      *    05  partner                 PIC  X(12).
           05  VBNr                    PIC  X(08).
           05  FILLER                  PIC  X(04).

           05  tz-01                   PIC  X(01).
           05  name                    PIC  X(40).
           05  tz-02                   PIC  X(01).
           05  name2                   PIC  X(40).
           05  tz-03                   PIC  X(01).
           05  vorname                 PIC  X(40).
           05  tz-04                   PIC  X(01).
           05  gebDatum                PIC  X(08).
           05  tz-05                   PIC  X(01).
           05  anrede                  PIC  X(01).
           05  tz-06                   PIC  X(01).
           05  titel                   PIC  X(30).
           05  tz-07                   PIC  X(01).
           05  staatsAngeh             PIC  X(03).
           05  tz-08                   PIC  X(01).
           05  typ                     PIC  X(03).
           05  tz-09                   PIC  X(01).
           05  gehoertZu               PIC  X(12).
           05  tz-10                   PIC  X(01).

      **** Feld "FunktionsKz" und "Direktion" eingefügt
           05  funktionsKz             PIC  X(03).
           05  tz-11                   PIC  X(01).
           05  direktion               PIC  X(08).
           05  tz-12                   PIC  X(01).
      ****
           05  anlageDatum             PIC  X(08).
           05  tz-13                   PIC  X(01).
           05  aenderungsDatum         PIC  X(08).
           05  tz-14                   PIC  X(01).
      *    *   Neu:1 Änderung:2 Löschung:9
           05  status-kz               PIC  X(01).


