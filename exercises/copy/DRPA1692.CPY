      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  09.02.2010
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2010 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRPA1692.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRPA1692.CPv  $
      *
      *   Rev 2.1   Jun 14 2010 10:01:52   t13ae23
      *APA01000: PIN-Änderungen AMV nach Aachen
      *
      *   Rev 2.0   Feb 09 2010 09:41:38   t13ae23
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRPA1692                                     *
      *    --------------------------------------------------------  *
      *    SCHNITTSTELLE FÜR DIE FUNKTION DRPA1692 DES BUFFER-OZP'S  *
      *    ZYPA0038                                                  *
      *    ( ERGEBNISMENGE X BEREITSTELLEN ,                         *
      *      BISHER BENUTZT VON PGPA0056 IM DIALOG PA03              *
      *      SERVICEFUNKTI0N PIN-ÄNDERUNG BEIM PRODUKTGEBER AMV)     *
      *    --------------------------------------------------------  *
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * APA01000!15.06.10!1.1 ! NEU                       ! RAHNER   *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER
      *    FÜR DIE FUNKTION DRPA1692
      *    AUSSER: DEM FUNKTIONSNAMEN ( IN DRPA1691 ),
      *            DEM ERR-ERROR-BEREICH UND
      *            DEM STANDARD-STATUS-BEREICH
      *            (JEWEILS SEPARATE COPYS)
      *
      *    FUNKTION DRPA1692 DES BUFFER-OZP ZYPA0038 ERMITTELT
      *    DIE ERGEBNISMENGE FÜR PA03, PINÄNDERUNG BEI AMV
      *
      ****************************************************************

       01  PI-DRPA1692.
           05  PI-DRPA1692-UFKT                    PIC  9(002).
           05  PI-DRPA1692-NAME-ZW-SPEICHER        PIC  X(008).
           05  PI-DRPA1692-LOG-SATZ-NR             PIC  9(005).

       01  PO-DRPA1692.
           05  PO-DRPA1692-RC                      PIC  X(03).
               88 PO-DRPA1692-RC-OK                       VALUE 'OK '.
               88 PO-DRPA1692-RC-EOF                      VALUE 'EOF'.
               88 PO-DRPA1692-RC-LEER                     VALUE 'LER'.
           05  PO-DRPA1692-ANZAHL-ERG-ZEILEN       PIC  9(05).
           05  PO-DRPA1692-LOG-SATZ-NR-1           PIC  9(05).

           05  PO-DRPA1692-ERGEBNISZEILE.
               10 PPIA-PGEB-GES            PIC S9(004) COMP.
               10 PPIA-VERTRAGSNUMMER      PIC X(015).
               10 PPIA-PIN                 PIC S9(009) COMP.
               10 PPIA-MELDE-KZ            PIC X.
               10 PPIA-MELDUNG-VON         PIC X(007).
               10 PPIA-MELDE-DATUM         PIC X(026).
      *
      ******* ENDE COPY-MEMBER **************************************


