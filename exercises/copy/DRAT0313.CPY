      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   drat0313.cpy  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:\MF\CPY\drat0313.cpv  $
      *
      *   Rev 2.1   Jan 11 2000 10:49:02   T13AE34
      *AAN00237 - MIS Antragsbereich
      *
      *   Rev 2.0   Mar 16 1999 09:57:22   P13SP20
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: DRAT0313 LIST ZXAT0017                       *
      *                          PRODUKTGEBERGESELLSCHAFTEN          *
      *    -------------------------------------------------------   *
      *    ERSTELLT: HEIDER                                          *
      *    DATUM:    19.03.1999                                      *
      ****************************************************************
      *    VERSION   01                                              *
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AAS00177!15.12.99!    !NEU                        !HEIDER    *
      */AAN00237!        !    !                           !          *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *                                                              *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRAT0313                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      *                                                              *
      *    FUNKTION: ERMITTELN EINER NOTIZ ZU EINER ART VON OBJEKT   *
      *                                                              *
      ****************************************************************
       01 PI-DRAT0313.
           05  PI-DRAT0313-UFKT                PIC  X(02).
      *   KANN BELEGT WERDEN MIT 1=LIST-FIRST, 2=LIST-NEXT, 3=QUIT
          05 PI-DRAT0313-AUSWAHL-KRITERIEN.
             10 FILLER                         PIC  X(1).

      *
       01 PO-DRAT0313.
          05 PO-DRAT0313-RC                    PIC  X(003).
             88 PO-DRAT0313-RC-OK                        VALUE 'OK '.
             88 PO-DRAT0313-RC-LEER                      VALUE 'LER'.
             88 PO-DRAT0313-RC-EOF                       VALUE 'EOF'.
          05 PO-DRAT0313-PGES-DATEN.
             10 PO-DRAT0313-KNR-PROD-GB-GES    PIC S9(4) USAGE COMP.
             10 PO-DRAT0313-PGES-DAT-VON       PIC  X(26).
             10 PO-DRAT0313-PGES-LAST-UPDATE   PIC  X(26).
             10 PO-DRAT0313-PGES-DAT-BIS       PIC  X(10).
             10 PO-DRAT0313-KNR-AUSL           PIC S9(4) USAGE COMP.
             10 PO-DRAT0313-KNR-BEWERT-ART     PIC S9(4) USAGE COMP.
             10 PO-DRAT0313-KTY-PROD-GB-GES    PIC S9(4) USAGE COMP.
             10 PO-DRAT0313-KTY-AUSL           PIC S9(4) USAGE COMP.
             10 PO-DRAT0313-KTY-BEWERT-ART     PIC S9(4) USAGE COMP.
      *
      ******** ENDE COPY-MEMBER **************************************
