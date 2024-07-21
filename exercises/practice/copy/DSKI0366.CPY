      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DSKI0366.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:\MF\CPY\DSKI0366.CPv  $
      *
      *   Rev 2.1   Mar 10 1999 12:07:14   I13PC48
      *Vorbereitung NTPut
      *
      *   Rev 2.0   Mar 03 1999 13:20:48   I13PC48
      *Version 2.0 nach Umstellung auf NT
      *
      *   Rev 1.0   30 Jun 1998 10:25:46   P13SP13
      *Initial revision.
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *                                                              *
      *    COPY-MEMBER: DSKI0366                                     *
      *                                                              *
      *    KONSTANTEN FÜR DIE AUSPRÄGUNGEN DER SUPPORTTABELLE        *
      *                                                              *
      *    Wohngebäude-Art - TCKI0366                                *
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
           05 K-EINFAMILIENHAUS              PIC  S9(4) BINARY VALUE 1.
           05 K-ZWEIFAMILIENHAUS             PIC  S9(4) BINARY VALUE 2.
           05 K-MEHRFAMILIENHAUS             PIC  S9(4) BINARY VALUE 3.
           05 K-WOHN-GESCHAEFTSHAUS          PIC  S9(4) BINARY VALUE 4.
           05 K-WOCHENENDE-FERIENHAUS        PIC  S9(4) BINARY VALUE 5.
           05 K-GARTENHAUS                   PIC  S9(4) BINARY VALUE 6.
           05 K-HAEUSER-DIVERSE01            PIC  S9(4) BINARY VALUE 7.
           05 K-HAEUSER-DIVERSE02            PIC  S9(4) BINARY VALUE 8.
           05 K-HAEUSER-DIVERSE03            PIC  S9(4) BINARY VALUE 9.
           05 K-WOHNGEBAEUDE                 PIC  S9(4) BINARY VALUE 10.
           05 K-LAGERHAUS                    PIC  S9(4) BINARY VALUE 11.
           05 K-GEBAEUDEART-NICHT-BEKANNT    PIC  S9(4) BINARY VALUE -2.

      ******************************************************************
      *          E n d e    Copy-Buch   DSKI0366                       *
      ******************************************************************
