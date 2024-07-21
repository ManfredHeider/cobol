      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VersionManager
      *            Datum      :  17.12.2018
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *    Copyright (c) 2018 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI3541.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRKI3541.CPv  $
      *
      *   Rev 2.1   Jan 30 2019 11:35:14   t13ex39
      *AKI01652: DeuBa TT für Zusätzliche Kontoinhaber
      *
      *   Rev 2.0   Dec 17 2018 15:10:52   t13ex39
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
      *    FÜR DIE FUNKTION DRKI3541
      *    AUSSER: DEM FUNKTIONSNAMEN,
      *            DEM ERR-ERROR-BEREICH UND
      *            DEM STANDARD-STATUS-BEREICH
      *            (JEWEILS SEPARATE COPYS)
      *
      *    FUNKTION: LIST
      *
      ****************************************************************
      *
       01  PI-DRKI3541.
           05 PI-DRKI3541-UFKT-CODE            PIC  9(002).
              88 PI-DRKI3541-UFKT-FIRST             VALUE 1.
              88 PI-DRKI3541-UFKT-NEXT              VALUE 2.
              88 PI-DRKI3541-UFKT-QUIT              VALUE 3.

           05  PI-DRKI3541-KEY.
               10 PI-DRKI3541-G48ZKVERTRAGSNR      PIC X(15) .

      *
       01  PO-DRKI3541.
           05 PO-DRKI3541-RC                   PIC  X(003).
              88 PO-DRKI3541-RC-OK                  VALUE 'OK '.
              88 PO-DRKI3541-RC-LEER                VALUE 'LER'.
              88 PO-DRKI3541-RC-EOF                 VALUE 'EOF'.
      *
           05  PO-DRKI3541-DATEN.


ANF   *        %M_PO_DATEN%
             10 PO-DRKI3541-G48ZKVERTRAGSNR PIC  X(015).
             10 PO-DRKI3541-G48ZKLFDNR      PIC S9(4) USAGE COMP.
             10 PO-DRKI3541-G48ZKANREDE     PIC  X(0001).
             10 PO-DRKI3541-G48ZKDELKZ      PIC  X(0001).
             10 PO-DRKI3541-G48ZKDEPOTKZ    PIC  X(0001).
             10 PO-DRKI3541-G48ZKGEBDAT     PIC  X(0008).
             10 PO-DRKI3541-G48ZKGESCHLECHT PIC  X(0001).
             10 PO-DRKI3541-G48ZKHAUSNR     PIC  X(0009).
             10 PO-DRKI3541-G48ZKLKZ        PIC  X(0003).
             10 PO-DRKI3541-G48ZKNACHNAME   PIC  X(0035).
             10 PO-DRKI3541-G48ZKORT        PIC  X(0028).
             10 PO-DRKI3541-G48ZKPLZ        PIC  X(0009).
             10 PO-DRKI3541-G48ZKPOSTFACH   PIC  X(0009).
             10 PO-DRKI3541-G48ZKSTR        PIC  X(0028).
             10 PO-DRKI3541-G48ZKTELNR      PIC  X(0009).
             10 PO-DRKI3541-G48ZKTELVOR     PIC  X(0006).
             10 PO-DRKI3541-G48ZKVORNAME    PIC  X(0035).
END   *




