      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DSKI0310.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:\MF\CPY\DSKI0310.CPv  $
      *
      *   Rev 2.1   Mar 10 1999 12:07:04   I13PC48
      *Vorbereitung NTPut
      *
      *   Rev 2.0   Mar 03 1999 13:20:40   I13PC48
      *Version 2.0 nach Umstellung auf NT
      *
      *   Rev 1.0   30 Jun 1998 10:24:28   P13SP13
      *Initial revision.
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *                                                              *
      *    COPY-MEMBER: DSKI0310                                     *
      *                                                              *
      *    Konstanten für die Ausprägungen der Supporttabelle        *
      *                                                              *
      *    Versicherungs-Gefahrencode - TCKI0310                     *
      *                                                              *
      ****************************************************************
      *    VERSION   01
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER  ! AENDERUNG                ! NAME     *
      * -------------------------------------------------------------*
      * AKI00001!18.08.98!01.01! Neuerstellung            !K-H Müller*
      * --------!--------!-----!--------------------------!----------*
      * KI-NEU  !        !     !                          !          *
      * --------!--------!-----!--------------------------!----------*
      * AKI00001! 1.09.98!01.02! GEFAHR-UNBEKANNT mit     !K-H Müller*
      *         !        !     ! Value '-2' (lt. Protokoll!          *
      *         !        !     ! v. 3.8.98) versehen      !          *
      * --------!--------!-----!--------------------------!----------*
      *         !        !     !                          !          *
      *         !        !     !                          !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************

       01  FILLER.
           05 K-GEFAHR-FEUER                  PIC S9(4) BINARY VALUE 1.
           05 K-GEFAHR-LEITUNGSWASSER         PIC S9(4) BINARY VALUE 2.
           05 K-GEFAHR-STURM                  PIC S9(4) BINARY VALUE 3.
           05 K-GEFAHR-HAGEL                  PIC S9(4) BINARY VALUE 4.
           05 K-GEFAHR-ELEMENTAR              PIC S9(4) BINARY VALUE 5.
           05 K-GEFAHR-EINBRUCHDIEBSTAHL      PIC S9(4) BINARY VALUE 6.
           05 K-GEFAHR-GLAS                   PIC S9(4) BINARY VALUE 8.
           05 K-GEFAHR-BAUWESEN               PIC S9(4) BINARY VALUE 9.
           05 K-GEFAHR-ELEKTRONIK             PIC S9(4) BINARY VALUE 10.
           05 K-GEFAHR-FEUER-BETR-UNTERB      PIC S9(4) BINARY VALUE 11.
           05 K-GEFAHR-TRANSPORT              PIC S9(4) BINARY VALUE 12.
           05 K-GEFAHR-LUFT                   PIC S9(4) BINARY VALUE 13.
           05 K-GEFAHR-MASCHINEN              PIC S9(4) BINARY VALUE 14.
           05 K-GEFAHR-MASCHINEN-BETR-UNTERB  PIC S9(4) BINARY VALUE 15.
           05 K-GEFAHR-UNBEKANNT              PIC S9(4) BINARY VALUE -2.

      ******************************************************************
      *          E n d e    Copy-Buch   DSKI0310                       *
      ******************************************************************
