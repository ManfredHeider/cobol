      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRAT0335.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:\MF\CPY\DRAT0335.CPv  $
      *
      *   Rev 2.1   29 May 2000 10:26:40   T13EX22
      *APA00090: KNR's für PEW-Status und Vertragsstand
      *
      *   Rev 2.0   Mar 16 1999 09:57:28   P13SP20
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: DRAT0335  KONSTANTEN FÜR                     *
      *                           VERTRAGSSTAND            KTY = 190 *
      *                                                              *
      *    -------------------------------------------------------   *
      *    ERSTELLT: MUND                                            *
      *    DATUM:    29.05.2000                                      *
      ****************************************************************
      *    VERSION   01
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * APA00090!29.05.00! 01 ! NEUERSTELLUNG             ! MUND     *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
       01  FILLER.
           05 K-KNR-PEW-NICHT-VORHANDEN   PIC S9(004) BINARY VALUE 1.
           05 K-KNR-PEW-VORHANDEN         PIC S9(004) BINARY VALUE 2.
           05 K-KNR-PEW-ABGERECHNET       PIC S9(004) BINARY VALUE 3.

