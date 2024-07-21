      *U---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  Januar 2001
      *            Kopf aus:  N:\PROFILE\KOPFCPY.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 2001 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRPA1497.CPY  $
      *    $Revision:   2.2  $
      *
      *    $Log:   L:/mf/CPY/DRPA1497.CPv  $
      *
      *   Rev 2.2   Nov 03 2006 13:31:26   t13ex25
      *APA00452: Erweiterung des PS auf 3 Stellen
      *
      *   Rev 2.1   20 Jul 2001 09:41:06   t13ex21
      *APA00133 - Neuerstellung
      *
      *   Rev 2.0   10 Apr 2001 08:42:24   p13sp16
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRPA1497  PEG-DATENSATZ DER DREBA (62)        *
      *    ---------------------------------------------------------  *
      *    ERSTELLT: STORCH                                           *
      *    DATUM:    21.06.01                                         *
      *****************************************************************
      *    VERSION
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * APA00133!21.06.01!2.1 ! NEUERSTELLUNG             ! STORCH   *
      * APA00452!03.11.06!2.2 ! Umstellung 3-stell. Prod. ! Jäger    *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *                                                              *
      *    DIESES COPY ENTHÄLT DEN AUFBAU DES PEG-DATENSATZES DES    *
      *    DRESDNER BANK-DTA'S.                                      *
      *    FELDER, DIE MIT "NR-" BEGINNEN, SIND FÜR DAISY NICHT      *
      *    RELEVANT.                                                 *
      ****************************************************************
      *
             30 DRESDNERBANK-DATENSATZ.
                35 FILLER                      PIC X.
                35 ABRECHNUNGSMONAT            PIC X(6).
                35 KZ-BEWEGUNG                 PIC X.
                35 ANREDE                      PIC X.
                35 NAME                        PIC X(30).
                35 STRASSE                     PIC X(30).
                35 ORT                         PIC X(30).
                35 GEBURTSDATUM                PIC X(8).
                35 BEREICH                     PIC X(3).
                35 KONTONUMMER                 PIC X(8).
                35 ABSCHLUSSDATUM              PIC X(8).
031106*         35 PROD-SCHL                   PIC X(6).
031106          35 TARIF                       PIC X(6).
                35 BEMESSUNGSGRUNDLAGE         PIC S9(11)V99  COMP-3.
                35 PROV-SCHL                   PIC X(4).
                35 PROV-SATZ                   PIC S9(3)V9999 COMP-3.
                35 PROV-BETRAG                 PIC S9(11)V99  COMP-3.
                35 VERMITTLER                  PIC X(18).
                35 PARTNERORD                  PIC X(20).
                35 STORNO                      PIC X(6).
                35 WAEHRUNG                    PIC 9(3).
                35 VERMITTLERNAME              PIC X(30).
                35 ERGAENZUNGSNR               PIC X(10).
                35 BETRIEBSSTELLE              PIC X(2).
      *
      ******** ENDE COPY-MEMBER **************************************
