      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  15.03.2019
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2019 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI3630.CPY  $
      *    $Revision:   2.5  $
      *
      *    $Log:   L:/mf/CPY/DRKI3630.CPv  $
      *
      *   Rev 2.5   Jul 28 2020 14:18:02   t13ex39
      *AKI01671: OZP
      *
      *   Rev 2.4   Jul 20 2020 09:12:10   t13ex39
      *AKI01671: OZP
      *
      *   Rev 2.3   Apr 29 2019 14:58:44   i13az81
      *AKI01671: OZP
      *
      *   Rev 2.2   Apr 29 2019 14:49:22   i13az81
      *AKI01671: OZP
      *
      *   Rev 2.1   Apr 11 2019 13:24:12   i13az81
      *AKI01671: OZP
      *
      *   Rev 2.0   Mar 15 2019 12:46:14 I13AZ81
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRKI3630                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE F?R DIE FUNKTION DRKI3630                    *
      *    --------------------------------------------------------   *
      ****************************************************************
      *   ERZEUGT MIT OZPGEN         15.03.2019 / 12:46:14 / I13AZ81
      *****************************************************************
      *    VERSION
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AKI01671!19.03.19!2.0 ! NEU                       ! N.RUBEN  *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER
      *    FÜR DIE FUNKTION DRKI3630
      *    AUSSER: DEM FUNKTIONSNAMEN,
      *            DEM ERR-ERROR-BEREICH UND
      *            DEM STANDARD-STATUS-BEREICH
      *            (JEWEILS SEPARATE COPYS)
      *
      *    >>>  UPDATE  <<<
      *
      ****************************************************************
      *
       01  PI-DRKI3630.
           05  PI-DRKI3630-KEY.


ANF   *        %M_PI_KEY%
             10 PI-DRKI3630-G48SPDVNR          PIC  X(0015).
             10 PI-DRKI3630-G48SPDUKONTONR     PIC  X(0002).
END   *

           05  PI-DRKI3630-NI.

ANF   *        %M_PI_DATEN_NI%
             15 FILLER                         PIC S9(4) COMP.
END   *

           05  PI-DRKI3630-DATEN.

ANF   *        %M_PI_DATEN%
             10 PI-DRKI3630-G48SPDBIC          PIC  X(0011).
             10 PI-DRKI3630-G48SPDDATOEFF      PIC  X(0008).
             10 PI-DRKI3630-G48SPDDELKZ        PIC  X(0001).
             10 PI-DRKI3630-G48SPDHABENZINS    PIC S9(2)V9(4) COMP-3.
             10 PI-DRKI3630-G48SPDIBAN         PIC  X(0034).
             10 PI-DRKI3630-G48SPDIBANSPK      PIC  X(0034).
             10 PI-DRKI3630-G48SPDKONTOART     PIC  X(0003).
             10 PI-DRKI3630-G48SPDMANDREF      PIC  X(0034).
             10 PI-DRKI3630-G48SPDNACHNAMEBZ   PIC  X(0028).
             10 PI-DRKI3630-G48SPDPIN          PIC  X(0009).
             10 PI-DRKI3630-G48SPDRATENHOEHE   PIC S9(12)V9(2) COMP-3.
             10 PI-DRKI3630-G48SPDSALDO        PIC S9(12)V9(2) COMP-3.
             10 PI-DRKI3630-G48SPDSCHLDAT      PIC  X(0008).
             10 PI-DRKI3630-G48SPDVERENDE      PIC  X(0008).
             10 PI-DRKI3630-G48SPDZAHLWEISE    PIC  X(0001).
END   *

      *  INITIALISIERUNG NULL-INDIKATOR
       01  PI-DRKI3630-NI1.
           05 PI-DRKI3630-INIT-TAB1            PIC S9(04) COMP VALUE -1
                                               OCCURS 1.
       01  PI-DRKI3630-NI7.
           05 PI-DRKI3630-INIT-TAB7            PIC S9(04) COMP VALUE -7
                                               OCCURS 1.
      *
       01  PO-DRKI3630.
           05  PO-DRKI3630-RC                  PIC  X(003).
               88 PO-DRKI3630-RC-OK            VALUE 'OK '.
               88 PO-DRKI3630-RC-NOT-FOUND     VALUE 'NFD'.
      *
      ******* ENDE COPY-MEMBER ***************************************





