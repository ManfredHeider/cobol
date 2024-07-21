      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VersionManager
      *            Datum      :  27.02.2019
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *    Copyright (c) 2019 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI3623.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRKI3623.CPv  $
      *
      *   Rev 2.1   Feb 27 2019 09:54:00   t13ex39
      *AKI01652: DeuBa TT für Zusätzliche Kontoinhaber
      *TABELLEN
      *
      *   Rev 2.0   Feb 27 2019 09:38:14   t13ex39
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRKI3541                                      *
      *    --------------------------------------------------------   *
      *    Cursor Full SELECT ZXKI0520 G48_ZUS_KUNDEN                 *
      *    --------------------------------------------------------   *
      ****************************************************************
      *    VERSION
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AKI01652!03.12.18!    ! Neuerstellung             ! who      *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER
      *    FÜR DIE FUNKTION DRKI3623
      *    AUSSER: DEM FUNKTIONSNAMEN,
      *            DEM ERR-ERROR-BEREICH UND
      *            DEM STANDARD-STATUS-BEREICH
      *            (JEWEILS SEPARATE COPYS)
      *
      *    FUNKTION: LIST
      *
      ****************************************************************
      *
       01  PI-DRKI3623.
           05 PI-DRKI3623-UFKT-CODE            PIC  9(002).
              88 PI-DRKI3623-UFKT-FIRST             VALUE 1.
              88 PI-DRKI3623-UFKT-NEXT              VALUE 2.
              88 PI-DRKI3623-UFKT-QUIT              VALUE 3.

           05  PI-DRKI3623-KEY.
               10 PI-DRKI3623-G48ZKVERTRAGSNR      PIC X(15) .

      *
       01  PO-DRKI3623.
           05 PO-DRKI3623-RC                   PIC  X(003).
              88 PO-DRKI3623-RC-OK                  VALUE 'OK '.
              88 PO-DRKI3623-RC-LEER                VALUE 'LER'.
              88 PO-DRKI3623-RC-EOF                 VALUE 'EOF'.
      *
           05  PO-DRKI3623-DATEN.


ANF   *        %M_PO_DATEN%
             10 PO-DRKI3623-G48ZKVERTRAGSNR PIC  X(015).
             10 PO-DRKI3623-G48ZKLFDNR      PIC S9(4) USAGE COMP.
             10 PO-DRKI3623-G48ZKANREDE     PIC  X(0001).
             10 PO-DRKI3623-G48ZKDELKZ      PIC  X(0001).
             10 PO-DRKI3623-G48ZKDEPOTKZ    PIC  X(0001).
             10 PO-DRKI3623-G48ZKGEBDAT     PIC  X(0008).
             10 PO-DRKI3623-G48ZKGESCHLECHT PIC  X(0001).
             10 PO-DRKI3623-G48ZKHAUSNR     PIC  X(0009).
             10 PO-DRKI3623-G48ZKLKZ        PIC  X(0003).
             10 PO-DRKI3623-G48ZKNACHNAME   PIC  X(0035).
             10 PO-DRKI3623-G48ZKORT        PIC  X(0028).
             10 PO-DRKI3623-G48ZKPLZ        PIC  X(0009).
             10 PO-DRKI3623-G48ZKPOSTFACH   PIC  X(0009).
             10 PO-DRKI3623-G48ZKSTR        PIC  X(0028).
             10 PO-DRKI3623-G48ZKTELNR      PIC  X(0009).
             10 PO-DRKI3623-G48ZKTELVOR     PIC  X(0006).
             10 PO-DRKI3623-G48ZKVORNAME    PIC  X(0035).
END   *



