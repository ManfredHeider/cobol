      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRPA0846.CPY  $
      *    $Revision:   2.0  $
      *
      *    $Log:   L:\MF\CPY\DRPA0846.CPv  $
      *
      *   Rev 2.0   Feb 27 1999 14:29:40   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.1   08 Aug 1998 11:33:54   T13AE34
      *AAN00167 - Vorvertragsnummer
      *
      *   Rev 1.0   14 Jan 1998 12:46:12   P13SP99
      *Initial revision.
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: DRPA0846 FUNKTION UMRECHNEN BRUTTO-RATIERLICH*
      *                          AUS ANTRAGSBEREICH IN NETTOBEITRAG  *
      *    -------------------------------------------------------   *
      *    ERSTELLT: HEIDER                                          *
      *    DATUM:    11.02.1998                                      *
      ****************************************************************
      *    VERSION   01                                              *
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AAN00167!11.02.98! 01 !NEU                        !HEIDER    *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *                                                              *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRPA0846                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      *                                                              *
      ****************************************************************
       01 PI-DRPA0846.
          05 PI-DRPA0846-DATEN.
             10 PI-DRPA0846-BRUTTO-BEITRAG-RAT   PIC S9(009)V9(002)
                                                        PACKED-DECIMAL.
             10 PI-DRPA0846-PGEB-GES             PIC S9(004)  BINARY.
             10 PI-DRPA0846-PROD-PROD-SCHL       PIC S9(004)  BINARY.
             10 PI-DRPA0846-STICHTAG             PIC  X(010).
             10 PI-DRPA0846-ZAHLWEISE-ANTRAG     PIC S9(004)  BINARY.
      *
       01 PO-DRPA0846.
          05 PO-DRPA0846-RC                      PIC  X(003).
             88 PO-DRPA0846-RC-OK                     VALUE 'OK '.
             88 PO-DRPA0846-RC-NOK                    VALUE 'NOK'.
         05 PO-DRPA0846-DATEN.
             10 PO-DRPA0846-NETTO-BEITRAG        PIC S9(009)V99
                                                        PACKED-DECIMAL.
