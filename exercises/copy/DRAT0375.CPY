      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  Januar 2001
      *            Kopf aus:  N:\PROFILE\KOPFCPY.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 2001 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRAT0375.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:\MF\CPY\DRAT0375.CPv  $
      *
      *   Rev 2.1   13 Jul 2001 12:03:06   t13ae34
      *AKI00202 - Pflege Batch-Fehlerhandling
      *
      *   Rev 2.0   19 Apr 2001 14:26:48   p13sp16
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: DRAT0375 FUNKTION                            *
      *                 AUFLISTEN DER ATTRIBUTE AUS FEHLERREATKION   *
      *    -------------------------------------------------------   *
      *    ERSTELLT: HEIDER                                          *
      *    DATUM:    JULI 2001                                       *
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AKI00202004.07.01!2.1 !NEU                        !HEIDER    *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *                                                              *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRAT0375                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      *                                                              *
      ****************************************************************
       01 PI-DRAT0375.
           05  PI-DRAT0375-UFKT                PIC  X(02).
      *   KANN BELEGT WERDEN MIT 1=LIST-FIRST, 2=LIST-NEXT, 3=QUIT
          05 PI-DRAT0375-AUSWAHL-KRITERIEN.
             10 PI-DRAT0375-FR-SYSCODE         PIC  X(2).
             10 PI-DRAT0375-FR-JOBNAME         PIC  X(8).
             10 PI-DRAT0375-FR-PROGRAMMNAME    PIC  X(8).

      *
       01 PO-DRAT0375.
          05 PO-DRAT0375-RC                    PIC  X(003).
             88 PO-DRAT0375-RC-OK                        VALUE 'OK '.
             88 PO-DRAT0375-RC-LEER                      VALUE 'LER'.
             88 PO-DRAT0375-RC-EOF                       VALUE 'EOF'.
          05 PO-DRAT0375-ERGEBNISZEILE.
             10 PO-DRAT0375-FR-FEHLERNUMMER    PIC S9(4) COMP.
             10 PO-DRAT0375-FR-FELDNAME        PIC  X(25).
             10 PO-DRAT0375-FR-RELEVANZ        PIC X.
             10 PO-DRAT0375-FR-REAKTION        PIC X.
             10 PO-DRAT0375-FR-REAKTIONHINWEIS PIC X(25).
             10 PO-DRAT0375-LETZTEAENDERUNG    PIC X(26).
             10 PO-DRAT0375-FR-USERID          PIC X(7).
      *
      ******** ENDE COPY-MEMBER **************************************
