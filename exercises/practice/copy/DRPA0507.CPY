      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRPA0507.CPY  $
      *    $Revision:   2.0  $
      *
      *    $Log:   L:\MF\CPY\DRPA0507.CPv  $
      *
      *   Rev 2.0   Feb 27 1999 14:29:10   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.1   07 Aug 1998 11:37:42   T13AE34
      *AAN00167 - Vorvertragsnummer
      *
      *   Rev 1.0   12 Sep 1997 16:58:46   P13SP99
      *Initial revision.
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: DRPA0507 FUNKTION LESEN KONDITION            *
      *                           FüR KUNDENBETREUER                 *
      *    -------------------------------------------------------   *
      *    ERSTELLT: BOHLINGER                                       *
      *    DATUM:    19.09.1997                                      *
      ****************************************************************
      *    VERSION   01                                              *
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AFPA0020!19.09.97! 01 !NEU                        !BOHLINGER *
      * AFPA0020!24.10.97!01.02!ALLE KONDITIONSSäTZE ER-  !          *
      *         !        !     !HALTEN VORZEICHEN         !          *
      *         !        !     !VERW. NESTED-COPY-STRECKE !BOHLINGER *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *                                                              *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRPA0507                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      *                                                              *
      ****************************************************************
       01 PI-DRPA0507.
          05 PI-DRPA0507-DATEN.
             10 PI-DRPA0507-PGEB-GES             PIC S9(004)  BINARY.
             10 PI-DRPA0507-PROD-PROD-SCHL       PIC S9(004)  BINARY.
             10 PI-DRPA0507-PRKO-KOND-NR         PIC S9(004)  BINARY.
      *
       01 PO-DRPA0507.
          05 PO-DRPA0507-RC                      PIC  X(003).
             88 PO-DRPA0507-RC-OK                     VALUE 'OK '.
             88 PO-DRPA0507-RC-NOT-FOUND              VALUE 'NFD'.
         05 PO-DRPA0507-DATEN.
             COPY DRPA0554.
      *
      ******** Ende Copy-Member **************************************
