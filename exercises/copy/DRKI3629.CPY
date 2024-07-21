      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  15.03.2019
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2019 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI3629.CPY  $
      *    $Revision:   2.5  $
      *
      *    $Log:   L:/mf/CPY/DRKI3629.CPv  $
      *
      *   Rev 2.5   Jul 28 2020 14:17:44   t13ex39
      *AKI01671: OZP
      *
      *   Rev 2.4   Jul 20 2020 09:11:54   t13ex39
      *AKI01671: OZP
      *
      *   Rev 2.3   Apr 29 2019 14:58:20   i13az81
      *AKI01671: OZP
      *
      *   Rev 2.2   Apr 29 2019 14:48:58   i13az81
      *AKI01671: OZP
      *
      *   Rev 2.1   Apr 11 2019 13:23:46   i13az81
      *AKI01671: OZP
      *
      *   Rev 2.0   Mar 15 2019 12:46:12 I13AZ81
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRKI3629                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DIE FUNKTION DRKI3629                      *
      *    --------------------------------------------------------   *
      ****************************************************************
      *   ERZEUGT MIT OZPGEN         15.03.2019 / 12:46:12 / I13AZ81
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
      *    FÜR DIE FUNKTION DRKI3629
      *    AUSSER: DEM FUNKTIONSNAMEN,
      *            DEM ERR-ERROR-BEREICH UND
      *            DEM STANDARD-STATUS-BEREICH
      *            (JEWEILS SEPARATE COPYS)
      *
      *    >>>  SELECT  <<<
      *
      ****************************************************************
      *
       01  PI-DRKI3629.
           05  PI-DRKI3629-KEY.


ANF   *        %M_PI_KEY%
             10 PI-DRKI3629-G48SPDVNR   PIC  X(0015).
             10 PI-DRKI3629-G48SPDUKONTONR   PIC  X(0002).
END   *

      *
       01  PO-DRKI3629.
           05  PO-DRKI3629-RC                     PIC  X(003).
               88 PO-DRKI3629-RC-OK               VALUE 'OK '.
               88 PO-DRKI3629-RC-NOT-FOUND        VALUE 'NFD'.
      *
ANF   *        %M_PO_DATEN_NI%
END   *

           05  PO-DRKI3629-DATEN.


ANF   *        %M_PO_DATEN%
             10 PO-DRKI3629-G48SPDBIC          PIC  X(0011).
             10 PO-DRKI3629-G48SPDDATOEFF      PIC  X(0008).
             10 PO-DRKI3629-G48SPDDELKZ        PIC  X(0001).
             10 PO-DRKI3629-G48SPDHABENZINS    PIC S9(2)V9(4) COMP-3.
             10 PO-DRKI3629-G48SPDIBAN         PIC  X(0034).
             10 PO-DRKI3629-G48SPDIBANSPK      PIC  X(0034).
             10 PO-DRKI3629-G48SPDKONTOART     PIC  X(0003).
             10 PO-DRKI3629-G48SPDMANDREF      PIC  X(0034).
             10 PO-DRKI3629-G48SPDNACHNAMEBZ   PIC  X(0028).
             10 PO-DRKI3629-G48SPDPIN          PIC  X(0009).
             10 PO-DRKI3629-G48SPDRATENHOEHE   PIC S9(12)V9(2) COMP-3.
             10 PO-DRKI3629-G48SPDSALDO        PIC S9(12)V9(2) COMP-3.
             10 PO-DRKI3629-G48SPDSCHLDAT      PIC  X(0008).
             10 PO-DRKI3629-G48SPDVERENDE      PIC  X(0008).
             10 PO-DRKI3629-G48SPDZAHLWEISE    PIC  X(0001).
END   *


      *
      ******* ENDE COPY-MEMBER ***************************************





