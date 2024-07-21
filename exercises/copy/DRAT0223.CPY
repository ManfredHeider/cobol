      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRAT0223.CPY  $
      *    $Revision:   2.2  $
      *
      *    $Log:   L:/mf/CPY/DRAT0223.CPv  $
      *
      *   Rev 2.2   Sep 02 2013 15:18:08   t13ae34
      *AAN01138: Ges. 16: Vorfinanzierte Provision
      *
      *   Rev 2.1   Jan 23 2012 11:46:22   t13ae34
      *AAN01006: Neues Produkt "UBR mit Pflege-Rente"
      *
      *   Rev 2.0   Feb 27 1999 11:57:36   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.1   07 Aug 1998 11:26:36   T13AE34
      *AAN00167 - Vorvertragsnummer
      *
      *   Rev 1.0   03 Dec 1997 15:47:50   P13SP99
      *Initial revision.
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: DRAT0223  KONSTANTEN FÜR PRODUKTGRUPPE       *
      *                           VERSICHERUNGSSTEUER      KTY = 152 *
      *                                                              *
      *    -------------------------------------------------------   *
      *    ERSTELLT: HEIDER                                          *
      *    DATUM:    11.02.1998                                      *
      ****************************************************************
      *    VERSION   01
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AAN00167!11.02.98! 01 ! NEUERSTELLUNG             ! HEIDER   *
      * AAN01138!28.08.13!2.2 ! DVAG-WIEN                 ! HEIDER   *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
       01  FILLER.
           05 K-KNR-VERSST-GLAS            PIC S9(004) BINARY VALUE 1.
           05 K-KNR-VERSST-HAFT            PIC S9(004) BINARY VALUE 2.
           05 K-KNR-VERSST-HAUSRAT         PIC S9(004) BINARY VALUE 3.
           05 K-KNR-VERSST-UNFALL          PIC S9(004) BINARY VALUE 4.
           05 K-KNR-VERSST-VW-OHNE-FEUER   PIC S9(004) BINARY VALUE 5.
           05 K-KNR-VERSST-VW-MIT-FEUER-UA PIC S9(004) BINARY VALUE 6.
           05 K-KNR-VERSST-VW-NUR-FEUER    PIC S9(004) BINARY VALUE 7.
280813     05 K-KNR-VERSST-RECHTSSCHUTZ    PIC S9(004) BINARY VALUE 8.
280813     05 K-KNR-VERSST-UNFALL-MIT-BRG  PIC S9(004) BINARY VALUE 9.
280813     05 K-KNR-VERSST-FEUER-WIEN      PIC S9(004) BINARY VALUE 10.
280813     05 K-KNR-VERSST-UNFALL-WIEN     PIC S9(004) BINARY VALUE 11.
280813     05 K-KNR-VERSST-SONSTIGE-WIEN   PIC S9(004) BINARY VALUE 12.
280813     05 K-KNR-VERSST-HAUSHALT-WIEN   PIC S9(004) BINARY VALUE 13.
280813     05 K-KNR-VERSST-KUEHLGUT-WIEN   PIC S9(004) BINARY VALUE 14.
280813     05 K-KNR-VERSST-KRANKENH-WIEN   PIC S9(004) BINARY VALUE 15.
