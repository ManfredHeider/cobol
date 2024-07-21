      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VersionManager
      *            Datum      :  07.09.2018
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *    Copyright (c) 2018 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI3449.CPY  $
      *    $Revision:   2.3  $
      *
      *    $Log:   L:/mf/CPY/DRKI3449.CPv  $
      *
      *   Rev 2.3   May 17 2019 11:49:02   t13ac20
      *AKI01600: Erweiterung Daisy
      *
      *   Rev 2.2   Nov 28 2018 17:33:12   t13ac20
      *AKI01600: Sperren & Löschen von Verträgen - DB2 neue Tabellen
      *
      *   Rev 2.1   Oct 31 2018 10:04:00   t13ac20
      *AKI01600: Sperren & Löschen von Verträgen - DB2 neue Tabellen
      *
      *   Rev 2.0   Sep 07 2018 12:01:42   t13ac20
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRKI3449                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DIE FUNKTION DRKI3449                    *
      *    --------------------------------------------------------   *
      *****************************************************************
      *    VERSION
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG | DATUM  |VER | AENDERUNG                 | NAME     *
      * -------------------------------------------------------------*
      * AKI01600|07.09.18|2.1 | NEUERSTELLUNG             | SWO      *
      * AKI01600|07.09.18|2.2 | FEHLER EINCHECKEN ED      | SWO      *
      * AKI01600|13.05.19|2.3 | NEUES FELD SPSDAISY-KZN   | SWO      *
      *         |        |    |                           |          *
      *         |        |    |                           |          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER
      *    FÜR DIE FUNKTION DRKI3449
      *    AUSSER: DEM FUNKTIONSNAMEN,
      *            DEM ERR-ERROR-BEREICH UND
      *            DEM STANDARD-STATUS-BEREICH
      *            (JEWEILS SEPARATE COPYS)
      *
      *    FUNKTION: SELECT LETZTER EINTRAG ZU SPSVERTRAGSNR
      *
      ****************************************************************
      *
       01  PI-DRKI3449.
           05 PI-DRKI3449-KEY.
              10 PI-DRKI3449-SPSVERTRAGSNR        PIC  X(015).
              10 PI-DRKI3449-SPSPARTGESELLID      PIC S9(004) BINARY.
      *
       01  PO-DRKI3449.
           05 PO-DRKI3449-RC                      PIC  X(003).
              88 PO-DRKI3449-RC-OK                       VALUE 'OK '.
              88 PO-DRKI3449-RC-NOT-FOUND                VALUE 'NFD'.
      *
           05 PO-DRKI3449-DATEN.
              10 PO-DRKI3449-SPSLFDNR             PIC S9(004) BINARY.
              10 PO-DRKI3449-SPSVERTRAGSID        PIC  X(013).
              10 PO-DRKI3449-SPSERSTELLTTST       PIC  X(026).
              10 PO-DRKI3449-SPSAKTION-KZN        PIC  X(001).
              10 PO-DRKI3449-LETZTEAENDERUNG      PIC  X(026).
              10 PO-DRKI3449-SPSDAISY-KZN         PIC  X(001).
      *
      ******* ENDE COPY-MEMBER ***************************************
