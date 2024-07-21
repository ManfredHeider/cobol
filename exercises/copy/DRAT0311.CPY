      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   drat0311.cpy  $
      *    $Revision:   2.2  $
      *
      *    $Log:   L:\MF\CPY\drat0311.cpv  $
      *
      *   Rev 2.2   Mar 16 2000 16:19:56   T13EX13
      *APA00067 Anpassung an Badenia +
      *Umstellung PGDF -> PGKF
      *
      *   Rev 2.1   Jan 06 2000 10:36:08   T13AE18
      *APA00068
      *
      *   Rev 2.0   Mar 16 1999 09:57:22   P13SP20
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: DRAT0311  KONSTANTEN FÜR KTY = 177           *
      *                           (DIALOGFELD-ZULAESIG)              *
      *                                                              *
      *    -------------------------------------------------------   *
      *    ERSTELLT: BOHLINGER                                       *
      *    DATUM:    02.12.1999                                      *
      ****************************************************************
      *    VERSION   01
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER  ! AENDERUNG                 ! NAME    *
      * -------------------------------------------------------------*
      * APA00068!02.12.99! 01  ! NEUERSTELLUNG             ! BOHLINGER
      * APA00067!14.03.00! 02  !UMSTELLUNG PGDF -> PGKF    !WILKENS  *
      *         !        !     !                           !         *
      *         !        !     !                           !         *
      *         !        !     !                           !         *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
       01  FILLER.
140300***  05 K-KNR-DIA-FELD-ANZEIGEN   PIC S9(004) BINARY VALUE 1.
140300***  05 K-KNR-DIA-FELD-AENDERN    PIC S9(004) BINARY VALUE 2.
140300     05 K-KNR-NPM-FELD-UNZULAESSIG  PIC S9(004) BINARY VALUE 1.
140300     05 K-KNR-NPM-FELD-ZULAESSIG    PIC S9(004) BINARY VALUE 2.
