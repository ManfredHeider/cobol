      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DSKI0062.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:\MF\CPY\DSKI0062.CPv  $
      *
      *   Rev 2.1   26 Sep 2000 16:39:40   T13EX12
      *AKI00139 - Neuer Kundenstatus
      *
      *   Rev 2.0   Feb 27 1999 12:59:00   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.1   09 Feb 1999 14:24:40   T13EX12
      *AKI00014 - Supporttabelle Kundenstatus
      *
      *   Rev 1.0   30 Jun 1998 10:23:34   P13SP13
      *Initial revision.
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *                                                              *
      *    COPY-MEMBER: DSKI0062                                     *
      *                                                              *
      *    KONSTANTEN FÜR DIE AUSPRÄGUNGEN DER SUPPORTTABELLE        *
      *                                                              *
      *    KUNDENSTATUS - TCKI0062                                   *
      *                                                              *
      ****************************************************************
      *    VERSION   01
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER  ! AENDERUNG                ! NAME     *
      * -------------------------------------------------------------*
      * AKI00001!17.10.98!01.01! NEUERSTELLUNG            !V.D.GOLTZ *
      * --------!--------!-----!--------------------------!----------*
      * AKI00139!14.08.00!02.01! Neuer Kundenstatus für   !V.D.GOLTZ *
      *         !        !     ! Kunden, die in Anwendung !          *
      *         !        !     ! nicht mehr verarb. werden!          *
      * --------!--------!-----!--------------------------!----------*
      *         !        !     !                          !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
       01  FILLER.
           05 K-ALTKUNDE                 PIC  X(001) VALUE 'A'.
           05 K-INTERESSENT              PIC  X(001) VALUE 'I'.
           05 K-KUNDE                    PIC  X(001) VALUE 'K'.
140800     05 K-LOESCHEN                 PIC  X(001) VALUE 'L'.
      *
      ********  ENDE COPY-MEMBER DSKI0062  ***********
