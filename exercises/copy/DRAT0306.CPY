      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRAT0306.CPY  $
      *    $Revision:   2.2  $
      *
      *    $Log:   L:\MF\CPY\DRAT0306.CPv  $
      *
      *   Rev 2.2   Nov 29 1999 17:39:54   T13AE34
      *AAN00236-PLZ
      *
      *   Rev 2.1   Nov 29 1999 11:00:06   T13AE34
      *AAN00236-PLZ
      *
      *   Rev 2.0   Mar 16 1999 09:57:20   P13SP20
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: DRAT0306 FUNKTION PLZ                        *
      *                 AUFLISTEN DER ORTSNAMEN ZU EINER PLZ         *
      *    -------------------------------------------------------   *
      *    ERSTELLT: HEIDER                                          *
      *    DATUM:    07.10.1999                                      *
      ****************************************************************
      *    VERSION   01                                              *
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AAT00005!19.03.99!    !NEU                        !HEIDER    *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *                                                              *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRAT0306                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      *                                                              *
      ****************************************************************
       01 PI-DRAT0306.
           05  PI-DRAT0306-UFKT                PIC  X(02).
      *   KANN BELEGT WERDEN MIT 1=LIST-FIRST, 2=LIST-NEXT, 3=QUIT
          05 PI-DRAT0306-AUSWAHL-KRITERIEN.
             10 PI-DRAT0306-PLZ-PLZ            PIC  X(005).

      *
       01 PO-DRAT0306.
          05 PO-DRAT0306-RC                     PIC  X(003).
             88 PO-DRAT0306-RC-OK                        VALUE 'OK '.
             88 PO-DRAT0306-RC-LEER                      VALUE 'LER'.
             88 PO-DRAT0306-RC-EOF                       VALUE 'EOF'.
          05 PO-DRAT0306-PLZ-DATEN.
             10 PO-DRAT0306-PLZ-ORT            PIC  X(035).
      *
      ******** ENDE COPY-MEMBER **************************************
