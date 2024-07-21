      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  Mai 2002
      *            Kopf aus:  N:\PROFILE\KOPFCPY.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2002 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRPA1575.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRPA1575.CPv  $
      *
      *   Rev 2.1   Jun 21 2004 16:39:38   t13ae34
      *Werkzeug für Auswertungen im eingereichten Geschäft
      *
      *   Rev 2.0   Jul 04 2002 15:31:10   p13sp16
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: DRPA1575 LIST ZXPA0004                       *
      *                          JEW. JÜNGSTEN EINTRAG PRODUKT       *
      *    -------------------------------------------------------   *
      *    ERSTELLT: HEIDER                                          *
      *    DATUM:    21.05.2004                                      *
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AAN00492!21.05.04!2.1 !NEU                        !HEIDER    *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
       01  PI-DRPA1575.
           05  PI-DRPA1575-UFKT                PIC  9(02).
           05  PI-DRPA1575-NAME-ZW-SPEICHER    PIC  X(08).
           05  PI-DRPA1575-AUSWAHL-KRITERIEN.
               10 FILLER                       PIC  X(01).
      *
       01  PO-DRPA1575.
           05  PO-DRPA1575-RC                  PIC  X(03).
               88 PO-DRPA1575-RC-OK                        VALUE 'OK '.
               88 PO-DRPA1575-RC-EOF                       VALUE 'EOF'.
               88 PO-DRPA1575-RC-LEER                      VALUE 'LER'.
           05  PO-DRPA1575-ANZAHL-ERG-ZEILEN   PIC  9(05).

           05  PO-DRPA1575-ERGEBNISZEILE.
               35 PO-DRPA1575-PGEB-GES         PIC S9(004) COMP.
               35 PO-DRPA1575-PROD-PROD-SCHL   PIC S9(004) COMP.
               35 PO-DRPA1575-PROD-BEGINN      PIC  X(010).
               35 PO-DRPA1575-PROD-ENDE        PIC  X(010).
               35 PO-DRPA1575-PROD-BEZ         PIC  X(040).
               35 PO-DRPA1575-PROD-BEZ-KURZ    PIC  X(010).
      *
      ******* ENDE COPY-MEMBER **************************************
