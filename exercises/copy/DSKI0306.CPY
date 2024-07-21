      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DSKI0306.CPY  $
      *    $Revision:   2.3  $
      *
      *    $Log:   L:/mf/CPY/DSKI0306.CPv  $
      *
      *   Rev 2.3   May 15 2017 12:46:08   t13ac13
      *AKI01516: G20 - Darstellung BAV-Kennzeichen - Copies und Pro
      *gramme
      *
      *   Rev 2.3   Apr 27 2017 08:30:22   t13ac13
      *AKI01516: "G20 - Darstellung BAV-Kennzeichen in den OKI-Vert
      *ragsdaten
      *
      *   Rev 2.2   Jul 03 2009 14:36:28   t13ex27
      *AKI00832: "Keine Angabe" in dem Feld >BAV< bei einem BAV-Ver
      *rag-Programme
      *
      *   Rev 2.1   Apr 28 2003 08:15:54   t13ae31
      *AKI00337 neue DTA-Felder Direktvers.
      *
      *   Rev 2.0   Mar 23 1999 15:53:28   T13EX33
      *aki00019 Produktionsübergabe
      *
      *   Rev 1.0   30 Jun 1998 10:24:20   P13SP13
      *Initial revision.
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *                                                              *
      *    COPY-MEMBER: DSKI0306                                     *
      *                                                              *
      *    KONSTANTEN FÜR DIE AUSPRÄGUNGEN DER SUPPORTTABELLE        *
      *                                                              *
      *    DIREKTVERSICHERER - TCKI0306                              *
      *                                                              *
      ****************************************************************
      *    VERSION   01
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER  ! AENDERUNG                ! NAME     *
      * -------------------------------------------------------------*
      * AKI00001!27.07.98!01.01! NEUERSTELLUNG            !HUEBNER   *
      * --------!--------!-----!--------------------------!----------*
      * AKI00337!11.04.03!     ! neue Schlüssel           ! P. Hübner*
      * AKI00832!10.06.09!2.1  ! Mischfinanzierte BAV     ! I.Frenzel*
      * AKI1516 !27.04.17!2.3  ! neue Schlüssel           ! M.Boost  *
      *         !        !     !                          !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
       01  FILLER.
           05 K-DIREKTVS-NEIN            PIC 9(02) VALUE  4.
           05 K-DIREKTVS-GEHALTUMW       PIC 9(02) VALUE  2.
           05 K-DIREKTVS-AGFIN           PIC 9(02) VALUE  1.
           05 K-DIREKTVS-KEINEANG        PIC 9(02) VALUE  3.
170427     05 K-DIREKTVS-31              PIC 9(02) VALUE  31.
170427     05 K-DIREKTVS-32              PIC 9(02) VALUE  32.
170427     05 K-DIREKTVS-33              PIC 9(02) VALUE  33.
170427     05 K-DIREKTVS-34              PIC 9(02) VALUE  34.
170427     05 K-DIREKTVS-35              PIC 9(02) VALUE  35.
170427     05 K-DIREKTVS-36              PIC 9(02) VALUE  36.
 
110403     05 K-BAV-ANFIN                PIC 9(02) VALUE  5.
110403     05 K-BAV-AGFIN                PIC 9(02) VALUE  6.
110403     05 K-BAV-FAVMG                PIC 9(02) VALUE  7.
110403     05 K-UKASSE-ANFIN             PIC 9(02) VALUE  8.
110403     05 K-UKASSE-AGFIN             PIC 9(02) VALUE  9.
110403     05 K-PK-ANFIN                 PIC 9(02) VALUE 10.
110403     05 K-PK-AGFIN                 PIC 9(02) VALUE 11.
100603     05 K-UKASSE-MIFIN             PIC 9(02) VALUE 12.
100603     05 K-BAV-MIFIN                PIC 9(02) VALUE 13.
100603     05 K-PK-MIFIN                 PIC 9(02) VALUE 14.
100603     05 K-RPK-PRIVFORT             PIC 9(02) VALUE 15.
100603     05 K-UKASSE-PRIVFORT          PIC 9(02) VALUE 16.
100603     05 K-BAV-PRIVFORT             PIC 9(02) VALUE 17.
100603     05 K-PK-PRIVFORT              PIC 9(02) VALUE 18.
100603     05 K-UKASSE-PFBZ              PIC 9(02) VALUE 19.
100603     05 K-BAV-PFBZ                 PIC 9(02) VALUE 20.
100603     05 K-PK-PFBZ                  PIC 9(02) VALUE 21.
100603     05 K-PK-RUECKDEK              PIC 9(02) VALUE 22.
270717     05 K-RIESTER-DIVE-AGFIN       PIC 9(02) VALUE 23.
270717     05 K-RIESTER-DIVE-ANFIN       PIC 9(02) VALUE 24.
270717     05 K-RIESTER-DIVE-MIFIN       PIC 9(02) VALUE 25.
270717     05 K-RIESTER-DIVE-PRIVAT      PIC 9(02) VALUE 26.
270717     05 K-RIESTER-DIVE-PRIVAT-FORT PIC 9(02) VALUE 27.
270717     05 K-RUECKDECK-PENSZUS-ANFIN  PIC 9(02) VALUE 28.
270717     05 K-RUECKDECK-PENSZUS-MIFIN  PIC 9(02) VALUE 29.
270717     05 K-RUECKDECK-PENSZUS-PRIVAT PIC 9(02) VALUE 30.
      *
      ********  ENDE COPY-MEMBER DSKI0306  ***********

