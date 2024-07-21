      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DSKI0008.CPY  $
      *    $Revision:   2.0  $
      *
      *    $Log:   L:\MF\CPY\DSKI0008.CPv  $
      *
      *   Rev 2.0   Feb 27 1999 12:58:56   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.1   09 Feb 1999 14:16:46   T13EX12
      *AKI00014 - Supporttabelle Bankkontoart
      *
      *   Rev 1.0   30 Jun 1998 10:22:32   P13SP13
      *Initial revision.
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *                                                              *
      *    COPY-MEMBER: DSKI0008                                     *
      *                                                              *
      *    KONSTANTEN FÜR DIE AUSPRÄGUNGEN DER SUPPORTTABELLE        *
      *                                                              *
      *    BANKKONTOART - TCKI0008                                   *
      *                                                              *
      ****************************************************************
      *    VERSION   01
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER  ! AENDERUNG                ! NAME     *
      * -------------------------------------------------------------*
      * AKI00001!02.07.98!01.01! NEUERSTELLUNG            !V.D.GOLTZ *
      * --------!--------!-----!--------------------------!----------*
      *         !22.08.98!01.02! 'nicht geliefert' in '-1'!K-H Müller*
      *         !        !     !        und               !          *
      *         !        !     ! 'unbekannt'       in '-2'!          *
      *         !        !     ! geändert.                !          *
      * --------!--------!-----!--------------------------!----------*
      *         !03.12.98!01.03! 'nicht geliefert' in     !v.d.Goltz *
      *         !        !     ! 'LEER' geändert.         !          *
      *         !        !     !                          !          *
      *         !        !     !                          !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
       01  FILLER.
           05 K-GIROKONTO                  PIC S9(004) BINARY VALUE 1.
           05 K-FESTGELDKONTO              PIC S9(004) BINARY VALUE 2.
031298*    05 K-KONTOART-NICHT-GELIEFERT   PIC S9(004) BINARY VALUE -1.
031298     05 K-KONTOART-LEER              PIC S9(004) BINARY VALUE -1.
           05 K-KONTOART-UNBEKANNT         PIC S9(004) BINARY VALUE -2.
      *
      ********  ENDE COPY-MEMBER DSKI0008  ***********
