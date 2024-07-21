      *---------------------------------------------------------------*
      *    Standard f?r alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DSKI0369.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:\MF\CPY\DSKI0369.CPv  $
      *
      *   Rev 2.1   14 Mar 2001 16:25:00   T13EX58
      *Neue Zahlungsweise eingefügt
      *Value '11'
      *
      *   Rev 2.0   Apr 07 1999 14:11:44   I13PC48
      *Version für NTPUT
      *
      *   Rev 1.2   Apr 07 1999 13:31:58   I13PC48
      *Zahlungsweise <3 = alle 3 Jahre> aufgenommen
      *
      *   Rev 1.1   09 Feb 1999 14:29:34   T13EX12
      *AKI00014 - Supporttabelle Zahlungsweise
      *
      *   Rev 1.0   30 Jun 1998 10:25:58   P13SP13
      *Initial revision.
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *                                                              *
      *    COPY-MEMBER: DSKI0369                                     *
      *                                                              *
      *    KONSTANTEN FšR DIE AUSPRŽGUNGEN DER SUPPORTTABELLE        *
      *                                                              *
      *    ZAHLUNGSWEISE - TCKI0369                                  *
      *                                                              *
      ****************************************************************
      *    VERSION   01
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER  ! AENDERUNG                ! NAME     *
      * -------------------------------------------------------------*
      * AKI00001!30.06.98!01.01! NEUERSTELLUNG            !V.D.GOLTZ *
      * --------!--------!-----!--------------------------!----------*
      * AKI00001! 1.09.98!01.02! ZAHL-WEISE-UNBEKANNT mit !K-H Müller*
      *         !        !     ! Value '-2' (lt. Protokoll!          *
      *         !        !     ! v. 3.8.98) versehen      !          *
      * --------!--------!-----!--------------------------!----------*
      * AKI00001!10.09.98!01.03! KURZFRISTIG UNTER 12 MON !Dobslaff  *
      *         !        !     ! Value '10' geändert,     !          *
      *         !        !     ! ZAHLWEISE LEER           !          *
      *         !        !     ! Value '-1' geändert,     !          *
      * --------!--------!-----!--------------------------!----------*
      * AKI00186!08.03.01!01.04! Neue Zahlungsweise       !Morfeld   *
      *         !        !     ! eingefügt Value '11'     !          *
      * --------!--------!-----!--------------------------!----------*
      *         !        !     !                          !          *
      *         !        !     !                          !          *
      *         !        !     !                          !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
       01  FILLER.
           05 K-JAEHRLICH                    PIC S9(004) BINARY VALUE 1.
           05 K-HALBJAEHRLICH                PIC S9(004) BINARY VALUE 2.
           05 K-ALLE-3-JAHRE                 PIC S9(004) BINARY VALUE 3.
           05 K-VIERTELJAEHRLICH             PIC S9(004) BINARY VALUE 4.
           05 K-ZWEIMONATLICH                PIC S9(004) BINARY VALUE 5.
           05 K-MONATLICH                    PIC S9(004) BINARY VALUE 6.
           05 K-EINMALZAHLUNG                PIC S9(004) BINARY VALUE 7.
           05 K-EINMALPRAEMIE                PIC S9(004) BINARY VALUE 8.
           05 K-KURZFRISTIG-UNTER-12-MON     PIC S9(004)
                                                        BINARY VALUE 10.
           05 K-JAEHRLICH-O-SKONTO           PIC S9(004)
                                                        BINARY VALUE 11.
           05 K-ZAHLWEISE-UNBEKANNT          PIC S9(004)
                                                        BINARY VALUE -2.
           05 K-ZAHLWEISE-LEER               PIC S9(004)
                                                        BINARY VALUE -1.
      *
      ********  ENDE COPY-MEMBER DSKI0369  ***********
