      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VersionManager
      *            Datum      :  25.06.2020
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *    Copyright (c) 2020 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI3814.CPY  $
      *    $Revision:   2.4  $
      *
      *    $Log:   L:/mf/CPY/DRKI3814.CPv  $
      *
      *   Rev 2.4   Oct 16 2020 10:41:46   t13ex39
      *AKI01790: Deutsche Bank - Depots KSDS--->TT
      *
      *   Rev 2.3   Oct 16 2020 10:35:16   t13ex39
      *AKI01790: Deutsche Bank - Depots KSDS--->TT
      *
      *   Rev 2.2   Oct 16 2020 09:45:48   t13ex39
      *AKI01790: Deutsche Bank - Depots KSDS--->TT
      *
      *   Rev 2.0   Jun 25 2020 11:19:46   t13ex39
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRKI3814                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DIE FUNKTION DRKI3814                    *
      *    --------------------------------------------------------   *
      ****************************************************************
      *   ERZEUGT MIT OZPGEN         25.06.2020 / 11:01:20 / T13EX39
      *****************************************************************
      *    VERSION
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER
      *    FÜR DIE FUNKTION DRKI3814
      *    AUSSER: DEM FUNKTIONSNAMEN,
      *            DEM ERR-ERROR-BEREICH UND
      *            DEM STANDARD-STATUS-BEREICH
      *            (JEWEILS SEPARATE COPYS)
      *
      *    >>>  LIST  <<<
      *
      ****************************************************************
      *
      *****************************************************************
      *    COPY-MEMBER: DRKI3814                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DIE FUNKTION DRKI3813                    *
      *    --------------------------------------------------------   *
      ****************************************************************
      *   ERZEUGT MIT OZPGEN         25.06.2020 / 11:01:20 / T13EX39
      *****************************************************************
      *    VERSION
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AKI01789!26.06.20!    ! NEU                       ! WHO      *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER
      *    FÜR DIE FUNKTION DRKI3813
      *    AUSSER: DEM FUNKTIONSNAMEN,
      *            DEM ERR-ERROR-BEREICH UND
      *            DEM STANDARD-STATUS-BEREICH
      *            (JEWEILS SEPARATE COPYS)
      *
      *    >>>  LIST  <<<
      *
      ****************************************************************
      *


        01  PI-DRKI3814.
            05 PI-DRKI3814-UFKT-CODE             PIC  9(0002).
               88 PI-DRKI3814-UFKT-FIRST              VALUE 1.
               88 PI-DRKI3814-UFKT-NEXT               VALUE 2.
               88 PI-DRKI3814-UFKT-QUIT               VALUE 3.

           05 PI-DRKI3814-KEY.
              10 PI-DRKI3814-G48MANDANT           PIC  S9(9) COMP.

        01  PO-DRKI3814.
            05 PO-DRKI3814-KEY.
             10 PO-DRKI3814-G48DSVNR              PIC X(15).
             10 PO-DRKI3814-G48DSUNTERNR          PIC X(02).
             10 PO-DRKI3814-G48DSWAEHRUNGSKZ      PIC X(03).

           05  PO-DRKI3814-RC                     PIC  X(003).
               88 PO-DRKI3814-RC-OK               VALUE 'OK '.
               88 PO-DRKI3814-RC-EOF              VALUE 'EOF'.
               88 PO-DRKI3814-RC-LEER              VALUE 'LER'.
      *
ANF   *        %M_PO_DATEN_NI%
END   *

           05  PO-DRKI3814-DATEN.
ANF   *        %M_PO_DATEN%
             10 PO-DRKI3814-G48DSANLAGEZIEL   PIC  X(0001).
             10 PO-DRKI3814-G48DSBIC   PIC  X(0011).
             10 PO-DRKI3814-G48DSDELKZ   PIC  X(0001).
             10 PO-DRKI3814-G48DSDEPOTART   PIC  X(0001).
             10 PO-DRKI3814-G48DSIBAN   PIC  X(0034).
             10 PO-DRKI3814-G48DSKONTOINH   PIC  X(0028).
             10 PO-DRKI3814-G48DSMANDREF   PIC  X(0035).
             10 PO-DRKI3814-G48DSNRGWBARPRO   PIC S9(6)V9(2) COMP-3.
             10 PO-DRKI3814-G48DSNRGWINKLBBA   PIC S9(11)V9(2) COMP-3.
             10 PO-DRKI3814-G48DSNRGWOHNEBBA   PIC S9(11)V9(2) COMP-3.
             10 PO-DRKI3814-G48DSNRGWOHNPRO   PIC S9(6)V9(2) COMP-3.
             10 PO-DRKI3814-G48DSOFFNUNGSDATUM   PIC  X(0008).
             10 PO-DRKI3814-G48DSRGWUDEPOT   PIC S9(9)V9(2) COMP-3.
             10 PO-DRKI3814-G48DSRISIKOKLASSE   PIC  X(0001).
             10 PO-DRKI3814-G48DSSCHLIESSDATUM   PIC  X(0008).
             10 PO-DRKI3814-G48DSSONDERKOND   PIC  X(0001).
             10 PO-DRKI3814-G48DSWPHGDAT   PIC  X(0008).
END   *


      *
      ******* ENDE COPY-MEMBER ***************************************



