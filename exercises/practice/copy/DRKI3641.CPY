      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VersionManager
      *            Datum      :  18.03.2019
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *    Copyright (c) 2019 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI3641.CPY  $
      *    $Revision:   2.5  $
      *
      *    $Log:   L:/mf/CPY/DRKI3641.CPv  $
      *
      *   Rev 2.5   Jul 28 2020 14:19:10   t13ex39
      *AKI01671: OZP
      *
      *   Rev 2.4   Jul 20 2020 09:13:36   t13ex39
      *AKI01671: OZP
      *
      *   Rev 2.3   Apr 29 2019 15:00:50   i13az81
      *AKI01671: OZP
      *
      *   Rev 2.2   Apr 29 2019 14:51:24   i13az81
      *AKI01671: OZP
      *
      *   Rev 2.1   Apr 11 2019 13:26:18   i13az81
      *AKI01671: OZP
      *
      *   Rev 2.0   Mar 18 2019 15:26:30   i13az81
      *
      *---------------------------------------------------------------*
      *    COPY-MEMBER: DRKI3641                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DIE FUNKTION DRKI3641                    *
      *    --------------------------------------------------------   *
      ****************************************************************
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
      *    FÜR DIE FUNKTION DRKI3641
      *    PIPO-COPY FÜR DRKI3641
      *    AUSSER: DEM FUNKTIONSNAMEN,
      *            DEM ERR-ERROR-BEREICH UND
      *            DEM STANDARD-STATUS-BEREICH
      *            (JEWEILS SEPARATE COPYS)
      *
      *    >>>  SELECT  <<<
      ****************************************************************
      *
      *
       01  PI-DRKI3641.
           05 PI-DRKI3641-UFKT-CODE             PIC  9(0002).
               88 PI-DRKI3641-UFKT-FIRST              VALUE 1.
               88 PI-DRKI3641-UFKT-NEXT               VALUE 2.
               88 PI-DRKI3641-UFKT-QUIT               VALUE 3.
           05 PI-DRKI3641-KEY.
               10 PI-DRKI3641-G48MANDANT        PIC  S9(9) COMP.

       01 PO-DRKI3641.
           05 PO-DRKI3641-KEY.
             10 PO-DRKI3641-G48SPDVNR           PIC  X(0015).
             10 PO-DRKI3641-G48SPDUKONTONR      PIC  X(0002).
 
          05 PO-DRKI3641-RC                     PIC X(003).
             88 PO-DRKI3641-RC-OK                  VALUE 'OK '.
             88 PO-DRKI3641-RC-LEER                VALUE 'LER'.
             88 PO-DRKI3641-RC-EOF                 VALUE 'EOF'.

           05  PO-DRKI3641-DATEN.
             10 PO-DRKI3641-G48SPDBIC           PIC  X(0011).
             10 PO-DRKI3641-G48SPDDATOEFF       PIC  X(0008).
             10 PO-DRKI3641-G48SPDDELKZ         PIC  X(0001).
             10 PO-DRKI3641-G48SPDHABENZINS     PIC S9(2)V9(4) COMP-3.
             10 PO-DRKI3641-G48SPDIBAN          PIC  X(0034).
             10 PO-DRKI3641-G48SPDIBANSPK       PIC  X(0034).
             10 PO-DRKI3641-G48SPDKONTOART      PIC  X(0003).
             10 PO-DRKI3641-G48SPDMANDREF       PIC  X(0034).
             10 PO-DRKI3641-G48SPDNACHNAMEBZ    PIC  X(0028).
             10 PO-DRKI3641-G48SPDPIN           PIC  X(0009).
             10 PO-DRKI3641-G48SPDRATENHOEHE    PIC S9(12)V9(2) COMP-3.
             10 PO-DRKI3641-G48SPDSALDO         PIC S9(12)V9(2) COMP-3.
             10 PO-DRKI3641-G48SPDSCHLDAT       PIC  X(0008).
             10 PO-DRKI3641-G48SPDVERENDE       PIC  X(0008).
             10 PO-DRKI3641-G48SPDZAHLWEISE     PIC  X(0001).
END   *


      *
      ******* ENDE COPY-MEMBER ***************************************





