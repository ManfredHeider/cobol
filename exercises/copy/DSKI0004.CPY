      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DSKI0004.CPY  $
      *    $Revision:   2.0  $
      *
      *    $Log:   L:\MF\CPY\DSKI0004.CPv  $
      *
      *   Rev 2.0   Feb 27 1999 12:58:56   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.1   21 Sep 1998 15:20:14   T13AE22
      *AKI00001 - VB-Schnittstelle
      *
      *   Rev 1.0   30 Jun 1998 10:22:28   P13SP13
      *Initial revision.
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *                                                              *
      *    COPY-MEMBER: DSKI0004                                     *
      *                                                              *
      *    KONSTANTEN FÜR DIE AUSPRÄGUNGEN DER SUPPORTTABELLE        *
      *                                                              *
      *    ANDREDE - TCKI0004                                        *
      *                                                              *
      ****************************************************************
      *    VERSION   01
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER  ! AENDERUNG                ! NAME     *
      * -------------------------------------------------------------*
      * AKI00001!25.06.98!01.01! NEUERSTELLUNG            ! SCHÄFER  *
      * --------!--------!-----!--------------------------!----------*
      * AKI00010!21.09.98!01.02!ÄNDERUNG DER DEFAULT-WERTE! SCHÄFER  *
      *         !        !     !'UNBEKANNT' UND 'LEER'    !          *
      * --------!--------!-----!--------------------------!----------*
      *         !        !     !                          !          *
      *         !        !     !                          !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
       01  FILLER.
           05 K-HERR                     PIC S9(004) BINARY VALUE 1.
           05 K-FRAU                     PIC S9(004) BINARY VALUE 2.
           05 K-FRAEULEIN                PIC S9(004) BINARY VALUE 3.
           05 K-EHELEUTE                 PIC S9(004) BINARY VALUE 4.
           05 K-FIRMA                    PIC S9(004) BINARY VALUE 5.
           05 K-ERBENGEMEINSCHAFT        PIC S9(004) BINARY VALUE 6.
           05 K-GEBRUEDER                PIC S9(004) BINARY VALUE 7.
           05 K-GESCHWISTER              PIC S9(004) BINARY VALUE 8.
210998**** 05 K-ANREDE-UNBEKANNT         PIC S9(004) BINARY VALUE 9.
210998**** 05 K-ANREDE-OHNE-ANGABE       PIC S9(004) BINARY VALUE 99.
210998     05 K-ANREDE-UNBEKANNT         PIC S9(004) BINARY VALUE -2.
210998     05 K-ANREDE-OHNE-ANGABE       PIC S9(004) BINARY VALUE -1.
      *
      ********  ENDE COPY-MEMBER DSKI0004  ***********
