      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DSKI0078.CPY  $
      *    $Revision:   2.0  $
      *
      *    $Log:   L:\MF\CPY\DSKI0078.CPv  $
      *
      *   Rev 2.0   Feb 27 1999 12:59:02   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.1   09 Feb 1999 14:25:26   T13EX12
      *AKI00014 - Supporttabelle Adressaendgrund
      *
      *   Rev 1.0   30 Jun 1998 10:24:00   P13SP13
      *Initial revision.
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *                                                              *
      *    COPY-MEMBER: DSKI0078                                     *
      *                                                              *
      *    KONSTANTEN FÜR DIE AUSPRÄGUNGEN DER SUPPORTTABELLE        *
      *                                                              *
      *    ADRESSAENDGRUND - TCKI0078                                *
      *                                                              *
      ****************************************************************
      *    VERSION   01
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER  ! AENDERUNG                ! NAME     *
      * -------------------------------------------------------------*
      * AKI00001!24.10.98!01.01! NEUERSTELLUNG            !V.D.GOLTZ *
      * --------!--------!-----!--------------------------!----------*
      * --------!--------!-----!--------------------------!----------*
      *         !        !     !                          !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
       01  FILLER.
           05 K-UMZUG                    PIC S9(004) BINARY VALUE 1.
           05 K-SCHREIBWEISE             PIC S9(004) BINARY VALUE 2.
           05 K-SONSTIGE-ADRESSAEND      PIC S9(004) BINARY VALUE 3.
           05 K-AENDGRUND-LEER           PIC S9(004) BINARY VALUE -1.
      *
      ********  ENDE COPY-MEMBER DSKI0078  ***********
