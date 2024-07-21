      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  Januar 1999
      *            Kopf aus:  N:\PROFILE\KOPFCPY.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1999 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRVB0459.CPY  $
      *    $Revision:   2.2  $
      *
      *    $Log:   L:/MF/CPY/DRVB0459.CPv  $
      *
      *   Rev 2.2   May 06 2002 10:12:10   t13ae23
      *AVB00600 - Anzeigen Ortsnamen -
      *
      *   Rev 2.1   16 Jun 2000 11:30:56   T13AE34
      *AVB00483-Dialog VB51 
      *
      *   Rev 2.0   Feb 07 2000 17:17:26   P13SP20
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRVB0459                                     *
      *    --------------------------------------------------------  *
      *    SCHNITTSTELLE FÜR DIE FUNKTION DRPA0459 DES BUFFER-OZP'S  *
      *    ZYVB0001                                                  *
      *    ( ERGEBNISMENGE X BEREITSTELLEN ,                         *
      *      BISHER BENUTZT VON PGVB0068 IM DIALOG VB51              *
      *      SUCHE VB NACH PLZ  )                                    *
      *    --------------------------------------------------------  *
      *    ERSTELLT: RAHNER
      *    DATUM:    22.03.2000                                      *
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AVB00485!22.03.00!2.1 ! NEU                       ! RAHNER   *
      * AVB00600!06.05.02!2.2 ! ANZEIGE ORTSNAME          ! RAHNER   *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER
      *    FÜR DIE FUNKTION DRVB0459
      *    AUSSER: DEM FUNKTIONSNAMEN ( IN DRVB0458 ),
      *            DEM ERR-ERROR-BEREICH UND
      *            DEM STANDARD-STATUS-BEREICH
      *            (JEWEILS SEPARATE COPYS)
      *
      *    FUNKTION DRVB0459 DES BUFFER-OZP ZYVB0001 ERMITTELT
      *    DIE ERGEBNISMENGE FÜR VB51, SUCHE VB NACH PLZ
      *
      ****************************************************************

       01  PI-DRVB0459.
           05  PI-DRVB0459-UFKT                    PIC  9(002).
           05  PI-DRVB0459-NAME-ZW-SPEICHER        PIC  X(008).
           05  PI-DRVB0459-LOG-SATZ-NR             PIC  9(005).

           05  PI-DRVB0459-AUSWAHL-KRITERIEN.
               10 PI-DRVB0459-PLZ                  PIC X(005).
               10 PI-DRVB0459-STUFE-VON            PIC X(004).
               10 PI-DRVB0459-STUFE-BIS            PIC X(004).
               10 PI-DRVB0459-EINHEITEN            PIC 9(007).
               10 PI-DRVB0459-DIRNR                PIC X(003).

       01  PO-DRVB0459.
           05  PO-DRVB0459-RC                      PIC  X(03).
               88 PO-DRVB0459-RC-OK                        VALUE 'OK '.
               88 PO-DRVB0459-RC-EOF                       VALUE 'EOF'.
               88 PO-DRVB0459-RC-LEER                      VALUE 'LER'.
           05  PO-DRVB0459-ANZAHL-ERG-ZEILEN       PIC  9(05).
           05  PO-DRVB0459-LOG-SATZ-NR-1           PIC  9(05).

           05  PO-DRVB0459-ERGEBNISZEILE.
               COPY DRVB0285.



