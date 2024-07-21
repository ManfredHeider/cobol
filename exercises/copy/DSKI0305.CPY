      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DSKI0305.CPY  $
      *    $Revision:   2.0  $
      *
      *    $Log:   L:\MF\CPY\DSKI0305.CPv  $
      *
      *   Rev 2.0   Feb 27 1999 12:59:04   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.1   09 Feb 1999 14:26:24   T13EX12
      *AKI00014 - Supporttabelle Beitragszustand
      *
      *   Rev 1.0   30 Jun 1998 10:24:16   P13SP13
      *Initial revision.
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *                                                              *
      *    COPY-MEMBER: DSKI0305                                     *
      *                                                              *
      *    KONSTANTEN FÜR DIE AUSPRÄGUNGEN DER SUPPORTTABELLE        *
      *                                                              *
      *    BUCHUNGSTEXT - TCKI0305                                   *
      *                                                              *
      ****************************************************************
      *    VERSION   01
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER  ! AENDERUNG                ! NAME     *
      * -------------------------------------------------------------*
      * AKI00001!23.09.98!01.01! NEUERSTELLUNG            !V.D.GOLTZ *
      * --------!--------!-----!--------------------------!----------*
      * AKI00001!01.10.98!01.02! Neue Buchungschlüssel    !V.D.GOLTZ *
      * --------!--------!-----!--------------------------!----------*
      *         !        !     !                          !          *
      *         !        !     !                          !          *
      *         !        !     !                          !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
       01  FILLER.
           05 K-AUSSCHUETTUNG            PIC S9(004) BINARY VALUE 1.
           05 K-KAUF                     PIC S9(004) BINARY VALUE 2.
           05 K-VERKAUF                  PIC S9(004) BINARY VALUE 3.
           05 K-UEBERTR-ZUGANG           PIC S9(004) BINARY VALUE 4.
           05 K-UEBERTR-ABGANG           PIC S9(004) BINARY VALUE 5.
011098     05 K-STORNIERUNG-KAUF         PIC S9(004) BINARY VALUE 6.
011098     05 K-STORNIERUNG-VERKAUF      PIC S9(004) BINARY VALUE 7.
011098     05 K-STORNIERUNG-ST-BUCH      PIC S9(004) BINARY VALUE 8.
           05 K-INFOSPERRE               PIC S9(004) BINARY VALUE 20.
           05 K-STORNO-AUSZAHLUNG        PIC S9(004) BINARY VALUE 21.
           05 K-UMBUCHUNG                PIC S9(004) BINARY VALUE 22.
           05 K-RUECKB-EINZUG            PIC S9(004) BINARY VALUE 23.
           05 K-ZWK-RUECKZAHLUNG         PIC S9(004) BINARY VALUE 24.
           05 K-SOLLZINSEN               PIC S9(004) BINARY VALUE 25.
           05 K-ZINSABSCHLAGST           PIC S9(004) BINARY VALUE 36.
           05 K-BUCHSCHL-UNBEKANNT       PIC S9(004) BINARY VALUE -2.
           05 K-BUCHSCHL-LEER            PIC S9(004) BINARY VALUE -1.
      *
      ********  ENDE COPY-MEMBER DSKI0305  ***********
