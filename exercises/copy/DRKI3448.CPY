      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VersionManager
      *            Datum      :  07.09.2018
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *    Copyright (c) 2018 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI3448.CPY  $
      *    $Revision:   2.3  $
      *
      *    $Log:   L:/mf/CPY/DRKI3448.CPv  $
      *
      *   Rev 2.3   May 17 2019 11:47:44   t13ac20
      *AKI01600: Erweiterung Daisy
      *
      *   Rev 2.2   Nov 28 2018 17:32:40   t13ac20
      *AKI01600: Sperren & Löschen von Verträgen - DB2 neue Tabellen
      *
      *   Rev 2.1   Oct 31 2018 10:03:14   t13ac20
      *AKI01600: Sperren & Löschen von Verträgen - DB2 neue Tabellen
      *
      *   Rev 2.0   Sep 07 2018 12:00:54   t13ac20
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRKI3448                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DIE FUNKTION DRKI3448                    *
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
      *    FÜR DIE FUNKTION DRKI3448
      *    AUSSER: DEM FUNKTIONSNAMEN,
      *            DEM ERR-ERROR-BEREICH UND
      *            DEM STANDARD-STATUS-BEREICH
      *            (JEWEILS SEPARATE COPYS)
      *
      *    FUNKTION: SELECT LETZTER EINTRAG ZU SPSVERTRAGSID
      *
      ****************************************************************
      *
       01  PI-DRKI3448.
           05 PI-DRKI3448-KEY.
              10 PI-DRKI3448-SPSVERTRAGSID        PIC  X(013).
      *
       01  PO-DRKI3448.
           05 PO-DRKI3448-RC                      PIC  X(003).
              88 PO-DRKI3448-RC-OK                       VALUE 'OK '.
              88 PO-DRKI3448-RC-NOT-FOUND                VALUE 'NFD'.
      *
           05 PO-DRKI3448-DATEN.
              10 PO-DRKI3448-SPSVERTRAGSNR        PIC  X(015).
              10 PO-DRKI3448-SPSPARTGESELLID      PIC S9(004) BINARY.
              10 PO-DRKI3448-SPSLFDNR             PIC S9(004) BINARY.
              10 PO-DRKI3448-SPSERSTELLTTST       PIC  X(026).
              10 PO-DRKI3448-SPSAKTION-KZN        PIC  X(001).
              10 PO-DRKI3448-LETZTEAENDERUNG      PIC  X(026).
              10 PO-DRKI3448-SPSDAISY-KZN         PIC  X(001).
      *
      ******* ENDE COPY-MEMBER ***************************************
