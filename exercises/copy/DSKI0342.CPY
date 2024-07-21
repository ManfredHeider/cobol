      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DSKI0342.CPY  $
      *    $Revision:   2.0  $
      *
      *    $Log:   L:\MF\CPY\DSKI0342.CPv  $
      *
      *   Rev 2.0   Feb 27 1999 12:59:08   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.1   09 Feb 1999 14:28:00   T13EX12
      *AKI00014 - Supporttabelle Verfuegung
      *
      *   Rev 1.0   30 Jun 1998 10:25:22   P13SP13
      *Initial revision.
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *                                                              *
      *    COPY-MEMBER: DSKI0342                                     *
      *                                                              *
      *    KONSTANTEN FÜR DIE AUSPRÄGUNGEN DER SUPPORTTABELLE        *
      *                                                              *
      *    VERFUEGUNG - TCKI0342                                     *
      *                                                              *
      ****************************************************************
      *    VERSION   01
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER  ! AENDERUNG                ! NAME     *
      * -------------------------------------------------------------*
      * AKI00001!30.06.98!01.01! NEUERSTELLUNG            !V.D.GOLTZ *
      * --------!--------!-----!--------------------------!----------*
      *         !        !     !                          !          *
      *         !        !     !                          !          *
      *         !        !     !                          !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
       01  FILLER.
           05 K-EINZELVERFUEGUNG           PIC  X(002) VALUE '1 '.
           05 K-GEMEINSCHAFTSVERFUEGUNG    PIC  X(002) VALUE '2 '.
           05 K-UEBER-GESETZL-VERTRETER    PIC  X(002) VALUE '3 '.
           05 K-VERFUEGUNG-UNBEKANNT       PIC  X(002) VALUE '-2'.
           05 K-VERFUEGUNG-LEER            PIC  X(002) VALUE '-1'.
      *
      ********  ENDE COPY-MEMBER DSKI0342  ***********
