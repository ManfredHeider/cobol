      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DSKI0001.CPY  $
      *    $Revision:   2.0  $
      *
      *    $Log:   L:\MF\CPY\DSKI0001.CPv  $
      *
      *   Rev 2.0   Feb 27 1999 12:58:54   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.1   10 Feb 1999 13:44:50   T13AE22
      *AKI00014
      *
      *   Rev 1.0   30 Jun 1998 10:22:24   P13SP13
      *Initial revision.
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *                                                              *
      *    COPY-MEMBER: DSKI0001                                     *
      *                                                              *
      *    KONSTANTEN FÜR DIE AUSPRÄGUNGEN DER SUPPORTTABELLE        *
      *                                                              *
      *    ADRESSART - TCKI0001                                      *
      *                                                              *
      ****************************************************************
      *    VERSION   01
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER  ! AENDERUNG                ! NAME     *
      * -------------------------------------------------------------*
      * AKI00001!25.06.98!01.01! NEUERSTELLUNG            ! SCHÄFER  *
      * --------!--------!-----!--------------------------!----------*
      *         !        !     !                          !          *
      *         !        !     !                          !          *
      *         !        !     !                          !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
       01  FILLER.
           05 K-HAUPTWOHNSITZ-NATP       PIC S9(004) BINARY VALUE 1.
           05 K-NEBENWOHNSITZ-NATP       PIC S9(004) BINARY VALUE 2.
           05 K-FERIENWOHNSITZ-NATP      PIC S9(004) BINARY VALUE 3.
           05 K-GESCHAEFTSADRESSE-NATP   PIC S9(004) BINARY VALUE 4.
           05 K-VERTRAGSADRESSE-NATP     PIC S9(004) BINARY VALUE 5.
           05 K-VERSANDADRESSE-NATP      PIC S9(004) BINARY VALUE 6.
           05 K-ZENTRALE-JURP            PIC S9(004) BINARY VALUE 7.
           05 K-ZWEIGSTELLE-JURP         PIC S9(004) BINARY VALUE 8.
           05 K-GESCHAEFTSADRESSE-JURP   PIC S9(004) BINARY VALUE 9.
           05 K-VERSANDADRESSE-JURP      PIC S9(004) BINARY VALUE 10.
           05 K-VERTRAGSADRESSE-JURP     PIC S9(004) BINARY VALUE 11.
           05 K-ADRESSART-UNBEKANNT-NATP PIC S9(004) BINARY VALUE 98.
           05 K-ADRESSART-UNBEKANNT-JURP PIC S9(004) BINARY VALUE 99.
      *
      ********  ENDE COPY-MEMBER DSKI0001  ***********
