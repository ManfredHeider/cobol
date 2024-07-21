      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRPA0589.CPY  $
      *    $Revision:   2.0  $
      *
      *    $Log:   L:\MF\CPY\DRPA0589.CPv  $
      *
      *   Rev 2.0   Feb 27 1999 14:29:18   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.1   07 Aug 1998 11:42:42   T13AE34
      *AAN00167 - Vorvertragsnummer
      *
      *   Rev 1.0   22 Oct 1997 13:13:10   P13PVCS
      *Initial revision.
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: DRPA0589 FUNKTION PRüFEN PRODUKT AUF         *
      *                           äHNLICHES PRODUKT                  *
      *    -------------------------------------------------------   *
      *    ERSTELLT: BOHLINGER                                       *
      *    DATUM:    31.10.1997                                      *
      ****************************************************************
      *    VERSION   01                                              *
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AFPA0020!31.10.97! 01 !NEU                        !BOHLINGER *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *                                                              *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRPA0589                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      *                                                              *
      ****************************************************************
       01 PI-DRPA0589.
          05 PI-DRPA0589-DATEN.
             10 PI-DRPA0589-PGEB-GES             PIC S9(004)  BINARY.
             10 PI-DRPA0589-PRUEF-PROD-SCHL      PIC S9(004)  BINARY.
             10 PI-DRPA0589-PROD-PROD-SCHL       PIC S9(004)  BINARY.
      *
       01 PO-DRPA0589.
          05 PO-DRPA0589-RC                      PIC  X(003).
             88 PO-DRPA0589-RC-AEPROD                 VALUE 'APR'.
             88 PO-DRPA0589-RC-KEIN-AEPROD            VALUE 'KAP'.
          05 PO-DRPA0589-DATEN.
             10 PO-DRPA0589-KNR-AEPROD           PIC S9(004) BINARY.
      *
      ******** Ende Copy-Member **************************************
