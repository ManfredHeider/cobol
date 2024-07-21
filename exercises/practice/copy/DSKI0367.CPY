      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DSKI0367.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:\MF\CPY\DSKI0367.CPv  $
      *
      *   Rev 2.1   Mar 10 1999 12:07:16   I13PC48
      *Vorbereitung NTPut
      *
      *   Rev 2.0   Mar 03 1999 13:20:50   I13PC48
      *Version 2.0 nach Umstellung auf NT
      *
      *   Rev 1.0   30 Jun 1998 10:25:50   P13SP13
      *Initial revision.
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *                                                              *
      *    COPY-MEMBER: DSKI0367                                     *
      *                                                              *
      *    KONSTANTEN FÜR DIE AUSPRÄGUNGEN DER SUPPORTTABELLE        *
      *                                                              *
      *    Bauart-Klasse - TCKI0367                                  *
      *                                                              *
      ****************************************************************
      *    VERSION   01
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER  ! AENDERUNG                ! NAME     *
      * -------------------------------------------------------------*
      * AKI00001!01.09.98!01.01! Neuerstellung            !K-H Müller*
      * --------!--------!-----!--------------------------!----------*
      *         !        !     !                          !          *
      *         !        !     !                          !          *
      *         !        !     !                          !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************

       01  FILLER.
           05 K-BAUART-KLASSE-I              PIC  S9(4) BINARY VALUE 1.
           05 K-BAUART-KLASSE-II             PIC  S9(4) BINARY VALUE 2.
           05 K-BAUART-KLASSE-III            PIC  S9(4) BINARY VALUE 3.
           05 K-BAUART-KLASSE-IV             PIC  S9(4) BINARY VALUE 4.
           05 K-BAUART-KLASSE-V              PIC  S9(4) BINARY VALUE 5.
           05 K-BAUART-KLASSE-I-ODER-II      PIC  S9(4) BINARY VALUE 6.
           05 K-BAUART-FERTIGHAUSGRUPPE0     PIC  S9(4) BINARY VALUE 7.
           05 K-BAUART-FERTIGHAUSGRUPPE1     PIC  S9(4) BINARY VALUE 8.
           05 K-BAUART-FERTIGHAUSGRUPPE2     PIC  S9(4) BINARY VALUE 9.
           05 K-BAUART-FERTIGHAUSGRUPPE3     PIC  S9(4) BINARY VALUE 10.
           05 K-BAUART-NICHT-BEKANNT         PIC  S9(4) BINARY VALUE -2.

      ******************************************************************
      *          E n d e    Copy-Buch   DSKI0367                       *
      ******************************************************************
