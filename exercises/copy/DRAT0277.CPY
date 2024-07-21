      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRAT0277.CPY  $
      *    $Revision:   2.6  $
      *
      *    $Log:   L:/mf/CPY/DRAT0277.CPv  $
      *
      *   Rev 2.6   Sep 02 2019 09:26:04   t13ex67
      *APA02796: Neuer Dialog für Korrekturen PRO-RATA Faktoren
      *
      *   Rev 2.5   Jul 16 2015 15:07:08   t13ae37
      *APA01932: ABU / Schnittstellen und Referenz
      *
      *   Rev 2.4   Feb 18 2002 14:23:54   t13ex43
      *FPA00352 : Korrekturbuchung für halbe Einheiten
      *
      *   Rev 2.3   Nov 26 2001 16:40:06   t13ex21
      *FPA00293 - Neue Ausprägung Buchungsgrund
      *
      *   Rev 2.2   Mar 25 1999 14:15:24   T13AE18
      *APA00012 Einführung neuer Konstanten
      *
      *   Rev 2.1   Mar 09 1999 12:07:16   T13EX44
      *NT-Umstellung nicht gelockter Files
      *
      *   Rev 2.0   Feb 27 1999 11:57:46   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.0   13 Jul 1998 09:28:32   P13SP13
      *Initial revision.
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: DRAT0277  KONSTANTEN FÜR                     *
      *                           BUCHUNGSGRUND            KTY = 155 *
      *                                                              *
      *    -------------------------------------------------------   *
      *    ERSTELLT: Parent                                          *
      *    DATUM:    01.08.1998                                      *
      ****************************************************************
      *    VERSION   01
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER  ! AENDERUNG                 ! NAME    *
      * -------------------------------------------------------------*
      * APA00012!01.08.98! 01  ! NEUERSTELLUNG             ! PARENT  *
      *         !22.10.98!01.01! NEU KNR-UMB-.. 5 BIS 9    !BOHLINGER*
      * APA00012!30.11.98! 02  ! GRUENDE FUER OPAL ERGAENZT! SCHEIDEL*
      * APA00012!26.02.96!02.02! NEU KNR-UMB-.. 12 BIS 16  !BOHLINGER*
      * FPA00293!06.11.01!02.03! NEUE AUSPRÄGUNG KNR 17    ! STORCH  *
      * FPA00352!11.02.02!02.04! NEUE AUSPRÄGUNG KNR 18    !KLEEMEIER*
      * APA01932!08.05.15!02.05! NEUE AUSPRÄGUNG KNR 19    !BUCH     *
      * APA02796!12.07.19!02.06! NEUE AUSPRÄGUNG KNR 20    ! STORCH  *
      *         !        !     !                           !         *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
       01  FILLER.
           05 K-KNR-AUTOM-PROV-ABR       PIC S9(004) BINARY VALUE 1.
           05 K-KNR-UMB-AEND-PEST        PIC S9(004) BINARY VALUE 2.
           05 K-KNR-UMB-AEND-PROV        PIC S9(004) BINARY VALUE 3.
           05 K-KNR-UMB-AEND-EVS         PIC S9(004) BINARY VALUE 4.
221098     05 K-KNR-UMB-AEND-PEST-DGV    PIC S9(004) BINARY VALUE 5.
221098     05 K-KNR-UMB-AEND-DGV         PIC S9(004) BINARY VALUE 6.
221098     05 K-KNR-UMB-AEND-PEST-VERTR  PIC S9(004) BINARY VALUE 7.
221098     05 K-KNR-UMB-AEND-VM-VERTR    PIC S9(004) BINARY VALUE 8.
221098     05 K-KNR-UMB-AEND-KDB-VERTR   PIC S9(004) BINARY VALUE 9.
301198     05 K-KNR-UMB-AEND-OPAL-AUTO   PIC S9(004) BINARY VALUE 10.
301198     05 K-KNR-UMB-AEND-OPAL-MANU   PIC S9(004) BINARY VALUE 11.
260299     05 K-KNR-UMB-AEND-PES-W       PIC S9(004) BINARY VALUE 12.
260299     05 K-KNR-UMB-AEND-PES-DGV-W   PIC S9(004) BINARY VALUE 13.
260299     05 K-KNR-UMB-AEND-EVS-EE      PIC S9(004) BINARY VALUE 14.
260299     05 K-KNR-UMB-AEND-EVS-A-PE-EE PIC S9(004) BINARY VALUE 15.
260299     05 K-KNR-UMB-AEND-EVS-U-PE    PIC S9(004) BINARY VALUE 16.
061101     05 K-KNR-UMB-SONDERKORREKTUR  PIC S9(004) BINARY VALUE 17.
110202     05 K-KNR-UMB-KORR-EINHEITEN   PIC S9(004) BINARY VALUE 18.
080515     05 K-KNR-KORR-UST-PRODUKT     PIC S9(004) BINARY VALUE 19.
120719     05 K-KNR-UMB-AEND-PRORATA     PIC S9(004) BINARY VALUE 20.
      *


