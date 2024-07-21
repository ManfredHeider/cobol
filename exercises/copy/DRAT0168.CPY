      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRAT0168.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRAT0168.CPv  $
      *
      *   Rev 2.1   Jul 16 2004 14:44:58   t13ae46
      *FPA00681: Korrektur Vorbewegungsermittlung WIK Generali LV
      *
      *   Rev 2.0   May 29 1999 08:03:24   T13EX25
      *APA00042, Neuerstellung V2.0
      *
      *   Rev 1.1   Mar 11 1999 11:47:24   T13EX25
      *Neue Ausprägung für OPAL-Meldung_ 
      *
      *   Rev 1.0   05 Sep 1997 09:20:28   P13SP99
      *Initial revision.
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: DRAT0168  KONSTANTEN FÜR                     *
      *                 PROVISIONSMELDUNG-BESTAND-ART      KTY = 135 *
      *                                                              *
      *    -------------------------------------------------------   *
      *    ERSTELLT: STORCH                                          *
      *    DATUM:    22.10.1997                                      *
      ****************************************************************
      *    VERSION   01
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AFPA0012!22.10.97! 01 ! NEUERSTELLUNG             ! STORCH   *
      * APA00012!11.03.99  02 ! Neue Ausprägung für OPAL  ! Jäger    *
      * FPA00681!13.07.04!2.1 ! NEUE AUSPRÄGUNG           ! WANG     *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
       01  FILLER.
           05 K-KNR-PG-PM-OHNE-DAISY-MELD  PIC S9(004) BINARY VALUE 1.
           05 K-KNR-PG-PM-MIT-DAISY-MELD   PIC S9(004) BINARY VALUE 2.
           05 K-KNR-DAISY-PM               PIC S9(004) BINARY VALUE 3.
110399     05 K-KNR-OPAL-PM                PIC S9(004) BINARY VALUE 4.
130704     05 K-KNR-DYNAMIK-BEW            PIC S9(004) BINARY VALUE 5.
