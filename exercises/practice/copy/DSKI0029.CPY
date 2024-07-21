      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DSKI0029.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DSKI0029.CPv  $
      *
      *   Rev 2.1   Oct 25 2004 12:07:06   t13ex27
      *AKI00399: Abo-Funktion (Einverständniserklärung) im Bestellv
      *organg
      *
      *   Rev 2.0   Feb 27 1999 12:58:58   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.2   29 Oct 1998 17:52:36   T13EX19
      *FKI00010 - Korrektur
      *
      *   Rev 1.1   12 Aug 1998 16:55:58   T13EX19
      *VB-SCHNITTSTELLE
      *
      *   Rev 1.0   30 Jun 1998 10:23:02   P13SP13
      *Initial revision.
      *
      *---------------------------------------------------------------*
      *---------------------------------------------------------------*
      ****************************************************************
      *                                                              *
      *    COPY-MEMBER: DSKI0029                                     *
      *                                                              *
      *    KONSTANTEN FÜR DIE AUSPRÄGUNGEN DER SUPPORTTABELLE        *
      *                                                              *
      *    KOMMEINRICHTUNG - TCKI0029                                   
      *                                                              *
      ****************************************************************
      *    VERSION   01
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER  ! AENDERUNG                ! NAME     *
      * -------------------------------------------------------------*
      * AKI00001!01.07.98!01.01! NEUERSTELLUNG            !ZGUR-VUJIC*
      * --------!--------!-----!--------------------------!----------*
      *         !22.08.98!01.02! 'Unbekannt'       in '-2'!K-H Müller*
      *         !        !     !     und                  !          *
      *         !        !     ! 'nicht-geliefert' in '-1'!          *
      *         !        !     ! geändert                 !          *
      * --------!--------!-----!--------------------------!----------*
      * AKI00399!26.05.04!2.1  ! Konstante für Homepage   !I. Frenzel*
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
           05 K-TELEFON-PRIVAT           PIC S9(004) BINARY VALUE 1.
           05 K-TELEFON-BUERO            PIC S9(004) BINARY VALUE 2.
           05 K-TELEFON-MOBIL            PIC S9(004) BINARY VALUE 3.
           05 K-FAX-PRIVAT               PIC S9(004) BINARY VALUE 4.
           05 K-FAX-BUERO                PIC S9(004) BINARY VALUE 5.
           05 K-E-MAIL-PRIVAT            PIC S9(004) BINARY VALUE 6.
           05 K-E-MAIL-BUERO             PIC S9(004) BINARY VALUE 7.
260504     05 K-HOMEPAGE                 PIC S9(004) BINARY VALUE 8.
           05 K-KOMM-NICHT-GELIEFERT     PIC S9(004) BINARY VALUE -1.
           05 K-KOMM-UNBEKANNT           PIC S9(004) BINARY VALUE -2.
      *
      ********  ENDE COPY-MEMBER DSKI0029  ***********
